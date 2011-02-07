{-# LANGUAGE FlexibleContexts  #-}
-- |
-- Module     : Data.Histogram
-- Copyright  : Copyright (c) 2009, Alexey Khudyakov <alexey.skladnoy@gmail.com>
-- License    : BSD3
-- Maintainer : Alexey Khudyakov <alexey.skladnoy@gmail.com>
-- Stability  : experimental
-- 
-- Generic immutable histograms. 
module Data.Histogram.Generic ( 
    -- * Data type
    Histogram
  , module Data.Histogram.Bin
  , histogram
  , histogramUO
    -- * Read histograms from string
  , readHistogram
  , readFileHistogram
    -- * Accessors
  , bins
  , histData
  , underflows
  , overflows
  , outOfRange
    -- ** Convert to other data types
  , asList
  , asVector
    -- * Splitting 2D histograms
  , sliceX
  , sliceY
    -- * Modify histogram
  , histMap
  , histMapBin
  , histZip
  , histZipSafe
  ) where

import Control.Applicative ((<$>),(<*>))
import Control.Arrow       ((***))
import Control.Monad       (ap)

import qualified Data.Vector.Generic         as G
import Data.Typeable        (Typeable1(..), Typeable2(..), mkTyConApp, mkTyCon)
import Data.Vector.Generic  (Vector,(!))
import Text.Read

import Data.Histogram.Bin
import Data.Histogram.Parse

----------------------------------------------------------------
-- Data type and smart constructors
----------------------------------------------------------------

-- | Immutable histogram. Histogram consists of binning algorithm,
--   optional number of under and overflows, and data. 
data Histogram v bin a = Histogram bin (Maybe (a,a)) (v a)
                         deriving (Eq)

-- | Create histogram from binning algorithm and vector with
-- data. Overflows are set to Nothing. 
--
-- Number of bins and vector size must match.
histogram :: (Vector v a, Bin bin) => bin -> v a -> Histogram v bin a
histogram b v | nBins b == G.length v = Histogram b Nothing v
              | otherwise             = error "histogram: number of bins and vector size doesn't match"


-- | Create histogram from binning algorithm and vector with data. 
--
-- Number of bins and vector size must match.
histogramUO :: (Vector v a, Bin bin) => bin -> Maybe (a,a) -> v a -> Histogram v bin a
histogramUO b uo v | nBins b == G.length v = Histogram b uo v
                   | otherwise             = error "histogram: number of bins and vector size doesn't match"


----------------------------------------------------------------
-- Instances & reading histograms from strings 
----------------------------------------------------------------

instance (Show a, Show (BinValue bin), Show bin, Bin bin, Vector v a) => Show (Histogram v bin a) where
    show h@(Histogram bin uo _) = "# Histogram\n" ++ showUO uo ++ show bin ++
                                  unlines (map showT $ asList h)
        where
          showT (x,y) = show x ++ "\t" ++ show y
          showUO (Just (u,o)) = "# Underflows = " ++ show u ++ "\n" ++
                                "# Overflows  = " ++ show o ++ "\n"
          showUO Nothing      = "# Underflows = \n" ++
                                "# Overflows  = \n"

instance Typeable1 v => Typeable2 (Histogram v) where
  typeOf2 h = mkTyConApp (mkTyCon "Data.Histogram.Generic.Histogram") [typeOf1 (histData h)]

-- Parse histogram header
histHeader :: (Read bin, Read a, Bin bin, Vector v a) => ReadPrec (v a -> Histogram v bin a)
histHeader = do
  keyword "Histogram"
  u   <- maybeValue "Underflows"
  o   <- maybeValue "Overflows"
  bin <- readPrec
  return $ Histogram bin ((,) `fmap` u `ap` o)

-- | Convert String to histogram. Histogram do not have Read instance
--   because of slowness of ReadP
readHistogram :: (Read bin, Read a, Bin bin, Vector v a) => String -> Histogram v bin a
readHistogram str = 
    let (h,rest) = case readPrec_to_S histHeader 0 str of
                     [x] -> x
                     _   -> error "Cannot parse histogram header"
        xs = map (unwords . tail) . filter (not . null) . map words . lines $ rest
    in h (G.fromList $ map read xs)

-- | Read histogram from file.
readFileHistogram :: (Read bin, Read a, Bin bin, Vector v a) => FilePath -> IO (Histogram v bin a)
readFileHistogram fname = readHistogram `fmap` readFile fname

----------------------------------------------------------------
-- Accessors & conversion
----------------------------------------------------------------

-- | Histogram bins
bins :: Histogram v bin a -> bin
bins (Histogram bin _ _) = bin

-- | Histogram data as vector
histData :: Histogram v bin a -> v a
histData (Histogram _ _ a) = a

-- | Number of underflows
underflows :: Histogram v bin a -> Maybe a
underflows (Histogram _ uo _) = fst <$> uo

-- | Number of overflows
overflows :: Histogram v bin a -> Maybe a
overflows (Histogram _ uo _) = snd <$> uo

-- | Underflows and overflows
outOfRange :: Histogram v bin a -> Maybe (a,a)
outOfRange (Histogram _ uo _) = uo

-- | Convert histogram to list.
asList :: (Vector v a, Bin bin) => Histogram v bin a -> [(BinValue bin, a)]
asList (Histogram bin _ arr) = map (fromIndex bin) [0..] `zip` G.toList arr

-- | Convert histogram to vector
asVector :: (Bin bin, Vector v a, Vector v (BinValue bin), Vector v (BinValue bin,a)) 
         => Histogram v bin a -> v (BinValue bin, a) 
asVector (Histogram bin _ arr) = G.zip (G.generate (nBins bin) (fromIndex bin) ) arr

----------------------------------------------------------------
-- Modify histograms
----------------------------------------------------------------

-- | fmap lookalike. It's not possible to create Functor instance
--   because of class restrictions
histMap :: (Vector v a, Vector v b) => (a -> b) -> Histogram v bin a -> Histogram v bin b
histMap f (Histogram bin uo a) = Histogram bin (fmap (f *** f) uo) (G.map f a)

-- | Apply function to histogram bins. Function must not change number of bins.
--   If it does error is thrown.
histMapBin :: (Bin bin, Bin bin') => (bin -> bin') -> Histogram v bin a -> Histogram v bin' a
histMapBin f (Histogram bin uo a)
    | nBins bin == nBins bin' = Histogram (f bin) uo a
    | otherwise               = error "Number of bins doesn't match"
    where
      bin' = bin

-- | Zip two histograms elementwise. Bins of histograms must be equal
--   otherwise error will be called.
histZip :: (Bin bin, Eq bin, Vector v a, Vector v b, Vector v c) =>
           (a -> b -> c) -> Histogram v bin a -> Histogram v bin b -> Histogram v bin c
histZip f (Histogram bin uo v) (Histogram bin' uo' v')
    | bin /= bin' = error "histZip: bins are different"
    | otherwise   = Histogram bin (f2 <$> uo <*> uo') (G.zipWith f v v')
      where
        f2 (x,x') (y,y') = (f x y, f x' y')

-- | Zip two histogram elementwise. If bins are not equal return `Nothing`
histZipSafe :: (Bin bin, Eq bin, Vector v a, Vector v b, Vector v c) =>
           (a -> b -> c) -> Histogram v bin a -> Histogram v bin b -> Maybe (Histogram v bin c)
histZipSafe f (Histogram bin uo v) (Histogram bin' uo' v')
    | bin /= bin' = Nothing
    | otherwise   = Just $ Histogram bin (f2 <$> uo <*> uo') (G.zipWith f v v')
      where
        f2 (x,x') (y,y') = (f x y, f x' y')


-- | Slice 2D histogram along Y axis. This function is fast because it does not require reallocations.
sliceY :: (Vector v a, Bin bX, Bin bY) => Histogram v (Bin2D bX bY) a -> [(BinValue bY, Histogram v bX a)]
sliceY (Histogram b _ a) = map mkSlice [0 .. ny-1]
    where
      (nx, ny) = nBins2D b
      mkSlice i = ( fromIndex (binY b) i
                  , Histogram (binX b) Nothing (G.slice (nx*i) nx a) )

-- | Slice 2D histogram along X axis.
sliceX :: (Vector v a, Bin bX, Bin bY) => Histogram v (Bin2D bX bY) a -> [(BinValue bX, Histogram v bY a)]
sliceX (Histogram b _ a) = map mkSlice [0 .. nx-1]
    where
      (nx, ny)  = nBins2D b
      mkSlice i = ( fromIndex (binX b) i
                  , Histogram (binY b) Nothing (mkArray i))
      mkArray x = G.generate ny (\y -> a ! (y*nx + x))
