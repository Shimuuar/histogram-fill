{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE DeriveDataTypeable #-}
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
  , HistIndex(..) 
  , histIndex
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
  , bmap
  , zip
  , zipSafe
    -- ** Type conversion
  , convert
  , convertBinning
    -- * Folding
  , foldl
  , bfoldl
    -- * Slicing & rebinning
  , slice
  , rebin
  , rebinFold
    -- * 2D histograms
    -- ** Slicing
  , sliceAlongX
  , sliceAlongY
  , listSlicesAlongX
  , listSlicesAlongY
    -- ** Reducing along axis
  , reduceX
  , reduceY
    -- * Lift histogram transform to 2D
  , liftX
  , liftY
  ) where

import Control.Applicative ((<$>),(<*>))
import Control.Arrow       ((***), (&&&))
import Control.Monad       (ap)
import Control.DeepSeq     (NFData(..))

import qualified Data.Vector.Generic         as G
import Data.Maybe           (fromMaybe)
import Data.Typeable        -- (Typeable(..),Typeable1(..),Typeable2(..),mkTyConApp,mkTyCon)
import Data.Vector.Generic  (Vector,(!))
import Text.Read
import Prelude       hiding (map,zip,foldl)
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

-- | Point inside histogram's domain. It could be either bin index or
--   bin value.
data HistIndex b
  = Index Int          -- ^ Index for a bin
  | Value (BinValue b) -- ^ Value
  deriving (Typeable)

-- | Convert 'HistIndex' to actual index
histIndex :: Bin b => b -> HistIndex b -> Int
histIndex _ (Index i) = i
histIndex b (Value x) = toIndex b x

-- | Create histogram from binning algorithm and vector with
-- data. Overflows are set to Nothing. 
--
-- Number of bins and vector size must match.
histogram :: (Vector v a, Bin bin) => bin -> v a -> Histogram v bin a
histogram b = histogramUO b Nothing

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
asVector :: (Bin bin, Vector v a, Vector v (BinValue bin,a))
         => Histogram v bin a -> v (BinValue bin, a)
asVector (Histogram bin _ arr) =
  G.generate (nBins bin) $ \i -> (fromIndex bin i, arr G.! i)



----------------------------------------------------------------
-- Modify histograms
----------------------------------------------------------------

-- | fmap lookalike. It's not possible to create Functor instance
--   because of type class context.
map :: (Vector v a, Vector v b) => (a -> b) -> Histogram v bin a -> Histogram v bin b
map f (Histogram bin uo a) = 
  Histogram bin (fmap (f *** f) uo) (G.map f a)

-- | Map histogram using bin value and content. Overflows and underflows are set to Nothing.
bmap :: (Vector v a, Vector v b, Bin bin)
     => (BinValue bin -> a -> b) -> Histogram v bin a -> Histogram v bin b
bmap f (Histogram bin _ vec) =
  Histogram bin Nothing $ G.imap (f . fromIndex bin) vec

-- | Zip two histograms elementwise. Bins of histograms must be equal
--   otherwise error will be called.
zip :: (Bin bin, BinEq bin, Vector v a, Vector v b, Vector v c) =>
       (a -> b -> c) -> Histogram v bin a -> Histogram v bin b -> Histogram v bin c
zip f ha hb = fromMaybe (error msg) $ zipSafe f ha hb
  where msg = "Data.Histogram.Generic.Histogram.histZip: bins are different"

-- | Zip two histogram elementwise. If bins are not equal return `Nothing`
zipSafe :: (Bin bin, BinEq bin, Vector v a, Vector v b, Vector v c) =>
           (a -> b -> c) -> Histogram v bin a -> Histogram v bin b -> Maybe (Histogram v bin c)
zipSafe f (Histogram bin uo v) (Histogram bin' uo' v')
  | binEq bin bin' = Just $ Histogram bin (f2 <$> uo <*> uo') (G.zipWith f v v')
  | otherwise      = Nothing
  where
    f2 (x,x') (y,y') = (f x y, f x' y')

-- | Convert between different vector types
convert :: (Vector v a, Vector w a)
        => Histogram v bin a -> Histogram w bin a
convert (Histogram bin uo vec) = Histogram bin uo (G.convert vec)

-- | Convert between binning types using 'ConvertBin' type class.
convertBinning :: (ConvertBin bin bin', Vector v a)
               => Histogram v bin a -> Histogram v bin' a
convertBinning (Histogram bin uo vec)
  | nBins bin == nBins bin' = Histogram bin' uo vec
  | otherwise               = error "Data.Histogram.Generic.convertBinning: invalid ConvertBin instance"
  where
    bin' = convertBin bin



----------------------------------------------------------------
-- Folding
----------------------------------------------------------------

-- | Strict fold over bin content in index order. Underflows and overflows are ignored.
foldl :: (Bin bin, Vector v a) => (b -> a -> b) -> b -> Histogram v bin a -> b
foldl f x0 (Histogram _ _ vec) =
  G.foldl' f x0 vec

-- | Strict fold over bin content in index order. Function is applied
--   to bin content and bin value. Underflows and overflows are ignored.
bfoldl :: (Bin bin, Vector v a) => (b -> BinValue bin -> a -> b) -> b -> Histogram v bin a -> b
bfoldl f x0 (Histogram bin _ vec) =
  G.ifoldl' (\acc -> f acc . fromIndex bin) x0 vec



----------------------------------------------------------------
-- Slicing and reducing histograms
----------------------------------------------------------------

-- | Slice histogram. Values/indices specify inclusive
--   variant. Under/overflows are discarded.
slice :: (SliceableBin bin, Vector v a)
      => HistIndex bin          -- ^ Lower inclusive bound
      -> HistIndex bin          -- ^ Upper inclusive bound
      -> Histogram v bin a      -- ^ Histogram to slice
      -> Histogram v bin a
slice a b (Histogram bin _ v) =
  Histogram (sliceBin i j bin) Nothing (G.slice i (j - i + 1) v)
  where
    i = histIndex bin a
    j = histIndex bin b

-- | Rebin histogram by joining @n@ adjacent bins.
rebin :: (MergeableBin bin, Vector v a)
      => CutDirection           -- ^ On which side bins should be discarded
      -> Int                    -- ^ Number of bins to join
      -> (a -> a -> a)          -- ^ Accumulation function
      -> Histogram v bin a
      -> Histogram v bin a
rebin dir k f =  rebinWorker dir k (G.foldl1' f)
{-# INLINE rebin #-}

-- | Rebin histogram by joining @n@ adjacent bins.
rebinFold :: (MergeableBin bin, Vector v a, Vector v b)
          => CutDirection       -- ^ On which side bins should be discarded
          -> Int                -- ^ Number of bins to join
          -> (b -> a -> b)      -- ^ Accumulation function
          -> b                  -- ^ Initial value
          -> Histogram v bin a
          -> Histogram v bin b
rebinFold dir k f x0 =  rebinWorker dir k (G.foldl' f x0)
{-# INLINE rebinFold #-}

rebinWorker :: (MergeableBin bin, Vector v a, Vector v b)
            => CutDirection
            -> Int
            -> (v a -> b)
            -> Histogram v bin a
            -> Histogram v bin b
{-# INLINE rebinWorker #-}
rebinWorker dir k f (Histogram bin _ vec)
  | G.length vec' /= nBins bin' = error "Data.Histogram.Generic.rebin: wrong MergeableBin instance"
  | otherwise                   = Histogram bin' Nothing vec'
  where
    bin' = mergeBins dir k bin
    vec' = G.generate n $ \i -> f (G.slice (off + i*k) k vec)
    n    = G.length vec `div` k
    off  = case dir of CutLower  -> G.length vec - n * k
                       CutHigher -> 0

----------------------------------------------------------------
-- 2D histograms
----------------------------------------------------------------

-- | Get slice of 2D histogram along X axis. This function is faster
--   than 'sliceAlongY' since no array reallocations is required
sliceAlongX :: (Vector v a, Bin bX, Bin bY)
            => Histogram v (Bin2D bX bY) a -- ^ 2D histogram
            -> HistIndex bY                -- ^ Position along Y axis
            -> Histogram v bX a
sliceAlongX (Histogram (Bin2D bX bY) _ arr) y
  | iy >= 0 && iy < ny = Histogram bX Nothing $ G.slice (nx * iy) nx arr
  | otherwise          = error "Data.Histogram.Generic.Histogram.sliceXatIx: bad index"
  where
    nx = nBins bX
    ny = nBins bY
    iy = histIndex bY y

-- | Get slice of 2D histogram along X axis
sliceAlongY :: (Vector v a, Bin bX, Bin bY)
            => Histogram v (Bin2D bX bY) a -- ^ 2D histogram
            -> HistIndex bX                -- ^ Position along X axis
            -> Histogram v bY a
sliceAlongY (Histogram (Bin2D bX bY) _ arr) x
  | ix >= 0 && ix < nx = Histogram bY Nothing $ G.generate ny (\iy -> arr ! (iy*nx + ix))
  | otherwise          = error "Data.Histogram.Generic.Histogram.sliceXatIx: bad index"
  where
    nx = nBins bX
    ny = nBins bY
    ix = histIndex bX x

-- | Slice 2D histogram along Y axis. This function is fast because it
--   does not require reallocations.
listSlicesAlongX :: (Vector v a, Bin bX, Bin bY)
                 => Histogram v (Bin2D bX bY) a
                 -> [(BinValue bY, Histogram v bX a)]
listSlicesAlongX h@(Histogram (Bin2D _ bY) _ _) =
  fmap (fromIndex bY &&& sliceAlongX h . Index) [0 .. nBins bY - 1]

-- | Slice 2D histogram along X axis.
listSlicesAlongY :: (Vector v a, Bin bX, Bin bY)
                 => Histogram v (Bin2D bX bY) a
                 -> [(BinValue bX, Histogram v bY a)]
listSlicesAlongY h@(Histogram (Bin2D bX _) _ _) =
  fmap (fromIndex bX &&& sliceAlongY h . Index) [0 .. nBins bX - 1]


-- | Reduce along X axis. Information about under/overlows is lost.
reduceX :: (Vector v a, Vector v b, Bin bX, Bin bY)
        => (Histogram v bX a -> b)      -- ^ Function to reduce single slice along X axis
        ->  Histogram v (Bin2D bX bY) a -- ^ 2D histogram
        ->  Histogram v bY b
reduceX f h@(Histogram (Bin2D _ bY) _ _) =
  Histogram bY Nothing $ G.generate (nBins bY) (f . sliceAlongX h . Index)


-- | Reduce along Y axis. Information about under/overflows is lost.
reduceY :: (Vector v a, Vector v b, Bin bX, Bin bY)
        => (Histogram v bY a -> b)     -- ^ Function to reduce histogram along Y axis
        -> Histogram v (Bin2D bX bY) a -- ^ 2D histogram
        -> Histogram v bX b
reduceY f h@(Histogram (Bin2D bX _) _ _) =
  Histogram bX Nothing $ G.generate (nBins bX) (f . sliceAlongY h . Index)

-- | Transform X slices of histogram.
liftX :: (Bin bX, Bin bY, Bin bX', BinEq bX', Vector v a, Vector v b)
      => (Histogram v bX a -> Histogram v bX' b)
      -> Histogram v (Bin2D bX  bY) a
      -> Histogram v (Bin2D bX' bY) b
liftX f hist@(Histogram (Bin2D _ by) _ _) =
  case f . snd <$> listSlicesAlongX hist of
    [] -> error "Data.Histogram.Generic.Histogram.liftX: zero size along Y"
    hs -> Histogram
          (Bin2D (bins (head hs)) by)
           Nothing
          (G.concat (histData <$> hs))

-- | Transform Y slices of histogram.
liftY :: (Bin bX, Bin bY, Bin bY', BinEq bY', Vector v a, Vector v b, Vector v Int)
      => (Histogram v bY a -> Histogram v bY' b)
      -> Histogram v (Bin2D bX bY ) a
      -> Histogram v (Bin2D bX bY') b
liftY f hist@(Histogram (Bin2D bx _) _ _) =
  case f . snd <$> listSlicesAlongY hist of
    [] -> error "Data.Histogram.Generic.Histogram.liftY: zero size along X"
    hs -> make hs
 where
   make hs = Histogram (Bin2D bx by') Nothing
           $ G.backpermute (G.concat (histData <$> hs)) (G.generate (nx*ny) join)
     where
       by'    = bins (head hs)
       nx     = nBins bx
       ny     = nBins by'
       join i = let (a,b) = i `quotRem` nx
                in  a + b * ny
