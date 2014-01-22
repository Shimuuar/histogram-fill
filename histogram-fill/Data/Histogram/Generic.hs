{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE CPP #-}
-- |
-- Module     : Data.Histogram
-- Copyright  : Copyright (c) 2009, Alexey Khudyakov <alexey.skladnoy@gmail.com>
-- License    : BSD3
-- Maintainer : Alexey Khudyakov <alexey.skladnoy@gmail.com>
-- Stability  : experimental
-- 
-- Generic immutable histograms. 
module Data.Histogram.Generic ( 
    -- * Immutable histograms
    Histogram
  , module Data.Histogram.Bin
    -- ** Constructors
  , histogram
  , histogramUO
    -- ** Conversion to other data types
  , asList
  , asVector
    -- * Serialization to strings
    -- $serialization
  , readHistogram
  , readFileHistogram
    -- * Accessors
  , bins
  , histData
  , underflows
  , overflows
  , outOfRange
    -- ** Indexing
  , HistIndex(..) 
  , histIndex
  , at
  , atV
  , atI
    -- * Transformations
  , map
  , bmap
  , mapData
  , zip
  , zipSafe
    -- ** Type conversion
  , convert
  , convertBinning
    -- * Folding
  , foldl
  , bfoldl
    -- ** Specialized folds
  , sum
  , minimum
  , minimumBy
  , maximum
  , maximumBy
  , minIndex
  , minIndexBy
  , maxIndex
  , maxIndexBy
  , minBin
  , minBinBy
  , maxBin
  , maxBinBy
    -- * Slicing & rebinning
  , slice
  , rebin
  , rebinFold
    -- * 2D histograms
    -- $hist2D
    -- ** Slicing
  , sliceAlongX
  , sliceAlongY
  , listSlicesAlongX
  , listSlicesAlongY
    -- ** Reducing along axis
  , reduceX
  , breduceX
  , reduceY
  , breduceY
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
import Data.Typeable
import Data.Vector.Generic  (Vector,(!))
import Text.Read
import Prelude       hiding (map,zip,foldl,sum,maximum,minimum)
import qualified Prelude    (zip)

import Data.Histogram.Bin
import Data.Histogram.Bin.Read



----------------------------------------------------------------
-- Data type & smart constructors & conversion
----------------------------------------------------------------

-- | Immutable histogram. Histogram consists of binning algorithm,
--   optional number of under and overflows, and data. Type parameter
--   have following meaning:
--
--   [@v@] type of vector used to store bin content.
--
--   [@bin@] binning. It should be instance of 'Bin'. Check that type class description for details.
--
--   [@a@] type of bin content.
data Histogram v bin a = Histogram !bin !(Maybe (a,a)) !(v a)
                         deriving (Eq)

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

-- | Convert histogram data to list.
asList :: (Vector v a, Bin bin) => Histogram v bin a -> [(BinValue bin, a)]
asList (Histogram bin _ arr) = 
  Prelude.zip (fromIndex bin <$> [0..]) (G.toList arr)

-- | Convert histogram data to vector
asVector :: (Bin bin, Vector v a, Vector v (BinValue bin,a))
         => Histogram v bin a -> v (BinValue bin, a)
asVector (Histogram bin _ arr) =
  G.generate (nBins bin) $ \i -> (fromIndex bin i, arr ! i)



----------------------------------------------------------------
-- Instances & reading histograms from strings 
----------------------------------------------------------------

-- $serialization
--
-- 'Show' instance is abused for serialization and produces human
-- readable data like that: 
--
-- > # Histogram
-- > # Underflows = 0
-- > # Overflows  = 88
-- > # BinI
-- > # Low  = 0
-- > # High = 9
-- > 0       99
-- > 1       91
-- > 2       95
-- > 3       81
-- > 4       92
-- > 5       105
-- > 6       90
-- > 7       79
-- > 8       91
-- > 9       89
--
-- It could be deserialize using 'readHistogram' function. 'Read'
-- instance coulde provided as well but it turned out to be
-- impractically slow.
--
-- Serialization with cereal package is provided by histogram-fill-cereal

instance (Show a, Show (BinValue bin), Show bin, Bin bin, Vector v a) => Show (Histogram v bin a) where
    show h@(Histogram bin uo _) = "# Histogram\n" ++ showUO uo ++ show bin ++
                                  unlines (fmap showT $ asList h)
        where
          showT (x,y) = show x ++ "\t" ++ show y
          showUO (Just (u,o)) = "# Underflows = " ++ show u ++ "\n" ++
                                "# Overflows  = " ++ show o ++ "\n"
          showUO Nothing      = "# Underflows = \n" ++
                                "# Overflows  = \n"


histTyCon :: String -> String -> TyCon
#if MIN_VERSION_base(4,4,0)
histTyCon = mkTyCon3 "histogram-fill"
#else
histTyCon m s = mkTyCon $ m ++ "." ++ s
#endif

instance Typeable1 v => Typeable2 (Histogram v) where
  typeOf2 h = mkTyConApp (histTyCon "Data.Histogram.Generic" "Histogram") [typeOf1 $ histData h]



-- | Vector do not supply 'NFData' instance so let just 'seq' it and
--   hope it's enough. Should be enough for unboxed vectors.
instance (NFData a, NFData bin, NFData (v a)) => NFData (Histogram v bin a) where
   rnf (Histogram bin uo vec) =
     rnf bin `seq` rnf uo `seq` rnf vec `seq` ()

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
-- Accessors
----------------------------------------------------------------

-- | Histogram bins
bins :: Histogram v bin a -> bin
bins (Histogram bin _ _) = bin

-- | Histogram data as vector
histData :: Histogram v bin a -> v a
histData (Histogram _ _ a) = a

-- | Number of underflows
underflows :: Histogram v bin a -> Maybe a
underflows h = fst <$> outOfRange  h

-- | Number of overflows
overflows :: Histogram v bin a -> Maybe a
overflows h = snd <$> outOfRange h

-- | Underflows and overflows
outOfRange :: Histogram v bin a -> Maybe (a,a)
outOfRange (Histogram _ uo _) = uo




-- | Point inside histogram's domain. It could be either bin index or
--   bin value. 'First' and 'Last' constructors are useful for
--   histogram slicing.
data HistIndex b
  = Index Int          -- ^ Index for a bin
  | Value (BinValue b) -- ^ Value
  | First              -- ^ Bin with index 0
  | Last               -- ^ Bin maximum index.
  deriving (Typeable)

-- | Convert 'HistIndex' to actual index
histIndex :: Bin b => b -> HistIndex b -> Int
histIndex _ (Index i) = i
histIndex b (Value x) = toIndex b x
histIndex _ First     = 0
histIndex b Last      = nBins b - 1

-- | Index histogtam.
at :: (Bin bin, Vector v a) => Histogram v bin a -> HistIndex bin -> a
at (Histogram bin _ v) i = v ! histIndex bin i

-- | Index histogram using bin value
atV :: (Bin bin, Vector v a) => Histogram v bin a -> BinValue bin -> a
atV h = at h . Value

-- | Index histogram using vector index
atI :: (Bin bin, Vector v a) => Histogram v bin a -> Int -> a
atI h = at h . Index



----------------------------------------------------------------
-- Transformation
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

mapData :: (Vector v a, Vector u b, Bin bin)
        => (v a -> u b) -> Histogram v bin a -> Histogram u bin b
mapData f (Histogram bin _ v)
  | G.length v /= G.length v' = error "Data.Histogram.Generic.Histogram.mapData: vector length changed"
  | otherwise                 = Histogram bin Nothing v'
  where v' = f v

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

-- | Sum contents of all bins
sum :: (Bin bin, Vector v a, Num a) => Histogram v bin a -> a
sum = foldl (+) 0


-- | Minimal bin content.
minimum :: (Bin bin, Vector v a, Ord a) => Histogram v bin a -> a
minimum = G.minimum . histData

-- | Minimal bin content using custom comparison.
minimumBy :: (Bin bin, Vector v a) => (a -> a -> Ordering) -> Histogram v bin a -> a
minimumBy f = G.minimumBy f . histData

-- | Maximal bin content
maximum :: (Bin bin, Vector v a, Ord a) => Histogram v bin a -> a
maximum = G.maximum . histData

-- | Maximal bin content using custom comparison.
maximumBy :: (Bin bin, Vector v a) => (a -> a -> Ordering) -> Histogram v bin a -> a
maximumBy f = G.maximumBy f . histData


-- | Index of a bin with minimal content
minIndex :: (Bin bin, Ord a, Vector v a) => Histogram v bin a -> Int
minIndex = G.minIndex . histData

-- | Index of a bin with minimal content using custom comparison.
minIndexBy :: (Bin bin, Ord a, Vector v a) => (a -> a -> Ordering) -> Histogram v bin a -> Int
minIndexBy f = G.minIndexBy f . histData

-- | Index of a bin with maximal content
maxIndex :: (Bin bin, Ord a, Vector v a) => Histogram v bin a -> Int
maxIndex = G.maxIndex . histData

-- | Index of a bin with maximal content using custom comparison.
maxIndexBy :: (Bin bin, Ord a, Vector v a) => (a -> a -> Ordering) -> Histogram v bin a -> Int
maxIndexBy f = G.maxIndexBy f . histData


-- | Value of a bin with minimal content
minBin :: (Bin bin, Ord a, Vector v a) => Histogram v bin a -> BinValue bin
minBin = minBinBy compare

-- | Value bin with minimal content using custom comparison.
minBinBy :: (Bin bin, Ord a, Vector v a) => (a -> a -> Ordering) -> Histogram v bin a -> BinValue bin
minBinBy f h = fromIndex (bins h) $ minIndexBy f h

-- | Value of a bin with maximal content
maxBin :: (Bin bin, Ord a, Vector v a) => Histogram v bin a -> BinValue bin
maxBin = maxBinBy compare

-- | Value of a bin with maximal content using custom comparison.
maxBinBy :: (Bin bin, Ord a, Vector v a) => (a -> a -> Ordering) -> Histogram v bin a -> BinValue bin
maxBinBy f h = fromIndex (bins h) $ maxIndexBy f h




----------------------------------------------------------------
-- Slicing and reducing histograms
----------------------------------------------------------------

-- | Slice histogram. Values/indices specify inclusive
--   variant. Under/overflows are discarded. If requested value falls
--   out of histogram range it will be truncated. Use 'First' or
--   'Last' constructor if you need slice from first or to last bin
--   correspondingly.
slice :: (SliceableBin bin, Vector v a)
      => HistIndex bin          -- ^ Lower inclusive bound
      -> HistIndex bin          -- ^ Upper inclusive bound
      -> Histogram v bin a      -- ^ Histogram to slice
      -> Histogram v bin a
slice a b (Histogram bin _ v) =
  Histogram (sliceBin i j bin) Nothing (G.slice i (j - i + 1) v)
  where
    i = max 0 $ histIndex bin a
    j = min n $ histIndex bin b
    n = nBins bin - 1

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

-- $hist2D
--
-- Data in 2D histograms is stored in row major order. This in fact
-- dictated by implementation of 'Bin2D'. So indices of bin are
-- arranged in following pattern:
-- 
-- >  0  1  2  3
-- >  4  5  6  7
-- >  8  9 10 11
--
-- Function from @AlongX@ family work with histogram slices along X
-- axis (as name suggest) which are contigous and therefor are
-- generally faster than @AlongY@ family.


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

-- | Reduce along X axis. Information about under/overlows is lost.
breduceX :: (Vector v a, Vector v b, Bin bX, Bin bY)
         => (BinValue bY -> Histogram v bX a -> b) -- ^ Function to reduce single slice along X axis
         ->  Histogram v (Bin2D bX bY) a           -- ^ 2D histogram
         ->  Histogram v bY b
breduceX f h@(Histogram (Bin2D _ bY) _ _) =
  Histogram bY Nothing $ G.generate (nBins bY) $ \i -> f (fromIndex bY i) $ sliceAlongX h (Index i)


-- | Reduce along Y axis. Information about under/overflows is lost.
reduceY :: (Vector v a, Vector v b, Bin bX, Bin bY)
        => (Histogram v bY a -> b)     -- ^ Function to reduce histogram along Y axis
        -> Histogram v (Bin2D bX bY) a -- ^ 2D histogram
        -> Histogram v bX b
reduceY f h@(Histogram (Bin2D bX _) _ _) =
  Histogram bX Nothing $ G.generate (nBins bX) (f . sliceAlongY h . Index)

-- | Reduce along Y axis. Information about under/overflows is lost.
breduceY :: (Vector v a, Vector v b, Bin bX, Bin bY)
         => (BinValue bX -> Histogram v bY a -> b) -- ^ Function to reduce histogram along Y axis
         -> Histogram v (Bin2D bX bY) a -- ^ 2D histogram
         -> Histogram v bX b
breduceY f h@(Histogram (Bin2D bX _) _ _) =
  Histogram bX Nothing $ G.generate (nBins bX) $ \i -> f (fromIndex bX i) $ sliceAlongY h (Index i)


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
