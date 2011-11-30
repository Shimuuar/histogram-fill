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
    -- * Modification
  , map
  , bMap
  , mapBin
  , zip
  , zipSafe
    -- * Folding
  , fold
  , binFold
    -- * Slicing histogram
  , sliceByIx
  , sliceByVal
    -- * Splitting 2D histograms
  , sliceXatIx
  , sliceYatIx
  , sliceX
  , sliceY
  , reduceX
  , reduceY
    -- * Modify histogram
  ) where

import Control.Applicative ((<$>),(<*>))
import Control.Arrow       ((***), (&&&))
import Control.Monad       (ap)
import Control.DeepSeq     (NFData(..))

import qualified Data.Vector.Generic         as G
import Data.Typeable        (Typeable1(..), Typeable2(..), mkTyConApp, mkTyCon)
import Data.Vector.Generic  (Vector,(!))
import Text.Read
import Prelude       hiding (map,zip)
import qualified Prelude    (zip)

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
histogram b v = histogramUO b Nothing v

-- | Create histogram from binning algorithm and vector with data. 
--
-- Number of bins and vector size must match.
histogramUO :: (Vector v a, Bin bin) => bin -> Maybe (a,a) -> v a -> Histogram v bin a
histogramUO b uo v 
  | nBins b == G.length v = Histogram b uo v
  | otherwise             = error "Data.Histogram.Generic.histogramUO: number of bins and vector size doesn't match"


----------------------------------------------------------------
-- Instances & reading histograms from strings 
----------------------------------------------------------------

instance (Show a, Show (BinValue bin), Show bin, Bin bin, Vector v a) => Show (Histogram v bin a) where
    show h@(Histogram bin uo _) = "# Histogram\n" ++ showUO uo ++ show bin ++
                                  unlines (fmap showT $ asList h)
        where
          showT (x,y) = show x ++ "\t" ++ show y
          showUO (Just (u,o)) = "# Underflows = " ++ show u ++ "\n" ++
                                "# Overflows  = " ++ show o ++ "\n"
          showUO Nothing      = "# Underflows = \n" ++
                                "# Overflows  = \n"

instance Typeable1 v => Typeable2 (Histogram v) where
  typeOf2 h = mkTyConApp (mkTyCon "Data.Histogram.Generic.Histogram") [typeOf1 (histData h)]

-- | Vector do not supply 'NFData' instance so let just 'seq' it and
--   hope it's enough. Should be enough for unboxed vectors.
instance (NFData a, NFData bin) => NFData (Histogram v bin a) where
   rnf (Histogram bin uo vec) = 
     rnf bin `seq` rnf uo `seq` seq vec ()

-- | If vector is a functor then histogram is functor as well
instance (Functor v) => Functor (Histogram v bin) where
  fmap f (Histogram bin uo vec) = Histogram bin (fmap (f *** f) uo) (fmap f vec)


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
        xs = fmap (unwords . tail) . filter (not . null) . fmap words . lines $ rest
    in h (G.fromList $ fmap read xs)

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

-- | Convert histogram data to list.
asList :: (Vector v a, Bin bin) => Histogram v bin a -> [(BinValue bin, a)]
asList (Histogram bin _ arr) = 
  Prelude.zip (fromIndex bin <$> [0..]) (G.toList arr)

-- | Convert histogram data to vector
asVector :: (Bin bin, Vector v a, Vector v (BinValue bin), Vector v (BinValue bin,a)) 
         => Histogram v bin a -> v (BinValue bin, a) 
asVector (Histogram bin _ arr) = G.zip (G.generate (nBins bin) (fromIndex bin) ) arr



----------------------------------------------------------------
-- Modify histograms
----------------------------------------------------------------

-- | fmap lookalike. It's not possible to create Functor instance
--   because of type class context.
map :: (Vector v a, Vector v b) => (a -> b) -> Histogram v bin a -> Histogram v bin b
map f (Histogram bin uo a) = 
  Histogram bin (fmap (f *** f) uo) (G.map f a)

-- | Map histogram usig bin value and content. Overflows and underflows are set to Nothing.
bMap :: (Vector v a, Vector v b, Bin bin)
         => (BinValue bin -> a -> b) -> Histogram v bin a -> Histogram v bin b
bMap f (Histogram bin uo vec) =
  Histogram bin Nothing $ G.imap (f . fromIndex bin) vec

-- | Apply function to histogram bins. Function must not change number of bins.
--   If it does error is thrown.
mapBin :: (Bin bin, Bin bin') => (bin -> bin') -> Histogram v bin a -> Histogram v bin' a
mapBin f (Histogram bin uo a)
  | nBins bin == nBins bin' = Histogram (f bin) uo a
  | otherwise               = error "Data.Histogram.Generic.Histogram.histMapBin: Number of bins doesn't match"
  where
    bin' = bin

-- | Zip two histograms elementwise. Bins of histograms must be equal
--   otherwise error will be called.
zip :: (Bin bin, Eq bin, Vector v a, Vector v b, Vector v c) =>
       (a -> b -> c) -> Histogram v bin a -> Histogram v bin b -> Histogram v bin c
zip f ha hb = maybe (error msg) id $ zipSafe f ha hb 
  where msg = "Data.Histogram.Generic.Histogram.histZip: bins are different"

-- | Zip two histogram elementwise. If bins are not equal return `Nothing`
zipSafe :: (Bin bin, Eq bin, Vector v a, Vector v b, Vector v c) =>
           (a -> b -> c) -> Histogram v bin a -> Histogram v bin b -> Maybe (Histogram v bin c)
zipSafe f (Histogram bin uo v) (Histogram bin' uo' v')
    | bin /= bin' = Nothing
    | otherwise   = Just $ Histogram bin (f2 <$> uo <*> uo') (G.zipWith f v v')
      where
        f2 (x,x') (y,y') = (f x y, f x' y')



----------------------------------------------------------------
-- Folding
----------------------------------------------------------------

-- | Fold over bin content in index order. Underflows and overflows are ignored.
fold :: (Bin bin, Vector v a) => (b -> a -> b) -> b -> Histogram v bin a -> b
fold f x0 (Histogram _ _ vec) = 
  G.foldl' f x0 vec

-- | Fold over bin content in index order. Function is applied to bin
--   content and bin value. Underflows and overflows are ignored.
binFold :: (Bin bin, Vector v a) => (b -> BinValue bin -> a -> b) -> b -> Histogram v bin a -> b
binFold f x0 (Histogram bin _ vec) = 
  G.ifoldl' (\acc -> f acc . fromIndex bin) x0 vec



----------------------------------------------------------------
-- Slicing and reducing histograms
----------------------------------------------------------------

-- | Slice histogram using indices.
sliceByIx :: (Bin1D bin, Vector v a) => Int -> Int -> Histogram v bin a -> Histogram v bin a
sliceByIx i j (Histogram b _ v) = 
  Histogram (sliceBin i j b) Nothing (G.slice i (j - i + 1) v)

-- | Slice histogram using bin values. Value will be included in range.
sliceByVal :: (Bin1D bin, Vector v a) => BinValue bin -> BinValue bin -> Histogram v bin a -> Histogram v bin a
sliceByVal x y h 
  | inRange b x && inRange b y = sliceByIx (toIndex b x) (toIndex b y) h
  | otherwise                  = error "Data.Histogram.Generic.Histogram.sliceByVal: Values are out of range"
    where
      b = bins h

-- | Get slice of 2D histogram along X axis
sliceXatIx :: (Vector v a, Bin bX, Bin bY)
           => Histogram v (Bin2D bX bY) a -- ^ 2D histogram
           -> Int                         -- ^ Bin index along Y axis
           -> Histogram v bX a
sliceXatIx (Histogram (Bin2D bX bY) _ arr) iy
  | iy >= 0 && iy < ny = Histogram bX Nothing $ G.slice (nx * iy) nx arr
  | otherwise          = error "Data.Histogram.Generic.Histogram.sliceXatIx: bad index"
  where
    nx = nBins bX
    ny = nBins bY

-- | Get slice of 2D histogram along X axis
sliceYatIx :: (Vector v a, Bin bX, Bin bY)
           => Histogram v (Bin2D bX bY) a -- ^ 2D histogram
           -> Int                         -- ^ Bin index along X axis
           -> Histogram v bY a
sliceYatIx (Histogram (Bin2D bX bY) _ arr) ix
  | ix >= 0 && ix < nx = Histogram bY Nothing $ G.generate ny (\iy -> arr ! (iy*nx + ix))
  | otherwise          = error "Data.Histogram.Generic.Histogram.sliceXatIx: bad index"
  where
    nx = nBins bX
    ny = nBins bY


-- | Slice 2D histogram along Y axis. This function is fast because it does not require reallocations.
sliceY :: (Vector v a, Bin bX, Bin bY)
       => Histogram v (Bin2D bX bY) a -> [(BinValue bY, Histogram v bX a)]
sliceY h@(Histogram (Bin2D _ bY) _ _) = fmap (fromIndex bY &&& sliceXatIx h) [0 .. nBins bY - 1]

-- | Slice 2D histogram along X axis.
sliceX :: (Vector v a, Bin bX, Bin bY)
       => Histogram v (Bin2D bX bY) a -> [(BinValue bX, Histogram v bY a)]
sliceX h@(Histogram (Bin2D bX _) _ _) = fmap (fromIndex bX &&& sliceYatIx h) [0 .. nBins bX - 1]


-- | Reduce along X axis
reduceX :: (Vector v a, Vector v b, Bin bX, Bin bY)
        => Histogram v (Bin2D bX bY) a -> (Histogram v bX a -> b) -> Histogram v bY b
reduceX h@(Histogram (Bin2D bX bY) _ arr) f =
  Histogram bY Nothing $ G.generate (nBins bY) (f . sliceXatIx h)


-- | Reduce along Y axis
reduceY :: (Vector v a, Vector v b, Bin bX, Bin bY)
        => Histogram v (Bin2D bX bY) a -> (Histogram v bY a -> b) -> Histogram v bX b
reduceY h@(Histogram (Bin2D bX bY) _ arr) f =
  Histogram bX Nothing $ G.generate (nBins bY) (f . sliceYatIx h)
