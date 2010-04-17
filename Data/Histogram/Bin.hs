
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs        #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE BangPatterns #-}
-- |
-- Module     : Data.Histogram.Bin
-- Copyright  : Copyright (c) 2009, Alexey Khudyakov <alexey.skladnoy@gmail.com>
-- License    : BSD3
-- Maintainer : Alexey Khudyakov <alexey.skladnoy@gmail.com>
-- Stability  : experimental
-- 
-- Binning algorithms. This is mapping from set of interest to integer
-- indices and approximate reverse. 

module Data.Histogram.Bin ( -- * Type classes
                            Bin(..)
                          , Bin1D(..)
                          , Indexable(..)
                          , Indexable2D(..)
                          -- * Bin types
                          -- ** Integer bins
                          , BinI(..)
                          , binI0
                          -- ** Indexed bins 
                          , BinIx(BinIx,unBinIx)
                          , binIx
                          -- ** Floating point bins
                          , BinF
                          , binF
                          , binFn
                          , binI2binF
                          , scaleBinF
                          -- *** Specialized for Double 
                          , BinD
                          , binD
                          , binDn
                          , binI2binD
                          , scaleBinD
                          -- ** Log scale point
                          , LogBinD 
                          , logBinD
                          -- ** 2D bins
                          , Bin2D(..)
                          , (><)
                          , nBins2D
                          , toIndex2D
                          , binX
                          , binY
                          , fmapBinX
                          , fmapBinY
                          -- ** 2D indexed bins
                          , BinIx2D (unBinIx2D)
                          , binIx2D
                          ) where

import Control.Monad
import Data.Histogram.Parse
import Text.Read (Read(..))

import GHC.Float (double2Int)
----------------------------------------------------------------
-- | Abstract binning algorithm. Following invariant is expected to hold: 
-- 
-- > toIndex . fromIndex == id
-- 
-- Reverse is not nessearily true. 
class Bin b where
    -- | Type of value to bin
    type BinValue b
    -- | Convert from value to index. No bound checking performed
    toIndex :: b -> BinValue b -> Int
    -- | Convert from index to value. 
    fromIndex :: b -> Int -> BinValue b 
    -- | Check whether value in range.
    inRange :: b -> BinValue b -> Bool
    -- | Total number of bins
    nBins :: b -> Int

----------------------------------------------------------------
-- | One dimensional binning algorithm
class Bin b => Bin1D b where
    -- | List of center of bins in ascending order.
    binsList :: b -> [BinValue b]
    -- | List of bins in ascending order.
    binsListRange :: b -> [(BinValue b, BinValue b)]

----------------------------------------------------------------
-- | Indexable is value which could be converted to and from Int
class Indexable a where
    -- | Convert value to index
    index :: a -> Int 
    -- | Convert index to value
    deindex :: Int -> a

instance Indexable Int where
    index   = id
    deindex = id

----------------------------------------------------------------
-- | Value which could be converted to/from (Int,Int) tuples
class Indexable2D a where
    -- | Convert value to index
    index2D :: a -> (Int,Int)
    -- | Convert index to value
    deindex2D :: (Int,Int) -> a

instance (Indexable a, Indexable b) => Indexable2D (a,b) where
    index2D   (x,y) = (index x,   index y)
    deindex2D (i,j) = (deindex i, deindex j)

----------------------------------------------------------------
-- Integer bin
----------------------------------------------------------------
-- | Integer bins. This is inclusive interval [from,to]
data BinI = BinI !Int !Int
            deriving Eq

-- | Construct BinI with n bins. Idexing starts from 0
binI0 :: Int -> BinI
binI0 n = BinI 0 (n-1)

instance Bin BinI where
    type BinValue BinI = Int
    toIndex   !(BinI base _) !x = x - base
    {-# INLINE toIndex #-}
    fromIndex !(BinI base _) !x = x + base
    inRange   !(BinI x y) i     = i>=x && i<=y
    {-# INLINE inRange #-}
    nBins     !(BinI x y) = y - x + 1

instance Bin1D BinI where
    binsList (BinI lo hi) = [lo .. hi]
    binsListRange b = zip (binsList b) (binsList b)

instance Show BinI where
    show (BinI lo hi) = unlines [ "# BinI"
                                , "# Low  = " ++ show lo
                                , "# High = " ++ show hi
                                ]
instance Read BinI where
    readPrec = keyword "BinI" >> liftM2 BinI (value "Low") (value "High")

----------------------------------------------------------------
-- Bins for indexables
----------------------------------------------------------------
-- | Binning for indexable values
newtype BinIx i = BinIx { unBinIx :: BinI }
                  deriving Eq

-- | Construct indexed bin
binIx :: Indexable i => i -> i -> BinIx i
binIx lo hi = BinIx $ BinI (index lo) (index hi)

instance Indexable i => Bin (BinIx i) where
    type BinValue (BinIx i) = i
    toIndex   (BinIx b) x = toIndex b (index x)
    fromIndex (BinIx b) i = deindex (fromIndex b i)
    inRange   (BinIx b) x = inRange b (index x)
    nBins (BinIx b) = nBins b

instance Indexable i => Bin1D (BinIx i) where
    binsList (BinIx b) = map deindex (binsList b)
    binsListRange b    = let bins = binsList b in zip bins bins

instance (Show i, Indexable i) => Show (BinIx i) where
    show (BinIx (BinI lo hi)) = unlines [ "# BinIx"
                                        , "# Low  = " ++ show (deindex lo :: i)
                                        , "# High = " ++ show (deindex hi :: i)
                                        ]
instance (Read i, Indexable i) => Read (BinIx i) where
    readPrec = keyword "BinIx" >> liftM2 binIx (value "Low") (value "High")

----------------------------------------------------------------
-- Floating point bin
----------------------------------------------------------------
-- | Floaintg point bins with equal sizes.
data BinF f where
    BinF :: RealFrac f => !f -> !f -> !Int -> BinF f 

instance Eq f => Eq (BinF f) where
    (BinF lo hi n) == (BinF lo' hi' n') = lo == lo'  && hi == hi' && n == n'
                                          
-- | Create bins.
binF :: RealFrac f => 
        f   -- ^ Lower bound of range
     -> Int -- ^ Number of bins
     -> f   -- ^ Upper bound of range
     -> BinF f
binF from n to = BinF from ((to - from) / fromIntegral n) n

-- | Create bins. Note that actual upper bound can differ from specified.
binFn :: RealFrac f =>
         f -- ^ Begin of range
      -> f -- ^ Size of step
      -> f -- ^ Approximation of end of range
      -> BinF f 
binFn from step to = BinF from step (round $ (to - from) / step)

-- | Convert BinI to BinF
binI2binF :: RealFrac f => BinI -> BinF f
binI2binF b@(BinI i _) = BinF (fromIntegral i) 1 (nBins b)

-- | 'scaleBinF a b' scales BinF using linear transform 'a+b*x'
scaleBinF :: RealFrac f => f -> f -> BinF f -> BinF f
scaleBinF a b (BinF base step n) 
    | b > 0     = BinF (a + b*base) (b*step) n
    | otherwise = error $ "scaleBinF: b must be positive (b = "++show b++")"

instance Bin (BinF f) where
    type BinValue (BinF f) = f 
    toIndex   !(BinF from step _) !x = floor $ (x-from) / step
    {-# INLINE toIndex #-}
    fromIndex !(BinF from step _) !i = (step/2) + (fromIntegral i * step) + from 
    inRange   !(BinF from step n) x  = x > from && x < from + step*fromIntegral n
    {-# INLINE inRange #-}
    nBins     !(BinF _ _ n) = n

instance Bin1D (BinF f) where
    binsList b@(BinF _ _ n) = map (fromIndex b) [0..n-1]
    binsListRange b@(BinF _ step _) = map toPair (binsList b)
        where
          toPair x = (x - step/2, x + step/2)

instance Show f => Show (BinF f) where
    show (BinF base step n) = unlines [ "# BinF"
                                      , "# Base = " ++ show base
                                      , "# Step = " ++ show step
                                      , "# N    = " ++ show n
                                      ]
instance (Read f, RealFrac f) => Read (BinF f) where
    readPrec = do
      keyword "BinF"
      base <- value "Base"
      step <- value "Step"
      n    <- value "N"
      return $ BinF base step n

----------------------------------------------------------------
-- Floating point bin /Specialized for Double
----------------------------------------------------------------
-- | Floaintg point bins with equal sizes.
data BinD = BinD Double Double Int

instance Eq BinD where
    (BinD lo hi n) == (BinD lo' hi' n') = lo == lo'  && hi == hi' && n == n'
                                          
-- | Create bins.
binD :: Double -- ^ Lower bound of range
     -> Int    -- ^ Number of bins
     -> Double -- ^ Upper bound of range
     -> BinD
binD from n to = BinD from ((to - from) / fromIntegral n) n

-- | Create bins. Note that actual upper bound can differ from specified.
binDn :: Double -- ^ Begin of range
      -> Double -- ^ Size of step
      -> Double -- ^ Approximation of end of range
      -> BinD
binDn from step to = BinD from step (round $ (to - from) / step)

-- | Convert BinI to BinF
binI2binD :: BinI -> BinD
binI2binD b@(BinI i _) = BinD (fromIntegral i) 1 (nBins b)

-- | 'scaleBinF a b' scales BinF using linear transform 'a+b*x'
scaleBinD :: Double -> Double -> BinD -> BinD
scaleBinD a b (BinD base step n) 
    | b > 0     = BinD (a + b*base) (b*step) n
    | otherwise = error $ "scaleBinF: b must be positive (b = "++show b++")"

-- Fast variant of flooor
floorD :: Double -> Int
floorD x | x < 0     = double2Int x - 1
         | otherwise = double2Int x
{-# INLINE floorD #-}

instance Bin BinD where
    type BinValue BinD = Double
    toIndex   !(BinD from step _) !x = floorD $ (x-from) / step
    {-# INLINE toIndex #-}
    fromIndex !(BinD from step _) !i = (step/2) + (fromIntegral i * step) + from 
    inRange   !(BinD from step n) x  = x > from && x < from + step*fromIntegral n
    {-# INLINE inRange #-}
    nBins     !(BinD _ _ n) = n

instance Bin1D BinD where
    binsList b@(BinD _ _ n) = map (fromIndex b) [0..n-1]
    binsListRange b@(BinD _ step _) = map toPair (binsList b)
        where
          toPair x = (x - step/2, x + step/2)

instance Show BinD where
    show (BinD base step n) = unlines [ "# BinD"
                                      , "# Base = " ++ show base
                                      , "# Step = " ++ show step
                                      , "# N    = " ++ show n
                                      ]
instance Read BinD where
    readPrec = do
      keyword "BinD"
      base <- value "Base"
      step <- value "Step"
      n    <- value "N"
      return $ BinD base step n


----------------------------------------------------------------
-- Log-scale bin
----------------------------------------------------------------
data LogBinD = LogBinD
               Double -- Low border
               Double -- Hi border
               Double -- Increment ratio
               Int    -- Number of bins
               deriving Eq

-- | Create log-scale bins. 
logBinD :: Double -> Int -> Double -> LogBinD
logBinD lo n hi = LogBinD lo hi ((hi/lo) ** (1 / fromIntegral n)) n

instance Bin LogBinD where
    type BinValue LogBinD = Double
    toIndex   !(LogBinD base _ step _) !x = floorD $ logBase step (x / base)
    {-# INLINE toIndex #-}
    fromIndex !(LogBinD base _ step _) !i = base * step ^ i
    inRange   !(LogBinD lo hi _ _) x  = x >= lo && x < hi
    {-# INLINE inRange #-}
    nBins     !(LogBinD _ _ _ n) = n

instance Show LogBinD where
    show (LogBinD lo hi step n) = 
        unlines [ "# LogBinD"
                , "# Lo   = " ++ show lo
                , "# Hi   = " ++ show hi
                , "# Step = " ++ show step
                , "# N    = " ++ show n
                ]

----------------------------------------------------------------
-- 2D bin
----------------------------------------------------------------

-- | 2D bins. binX is binning along X axis and binY is one along Y axis. 
data Bin2D binX binY = Bin2D binX binY
                       deriving Eq

-- | Alias for 'Bin2D'.
(><) :: binX -> binY -> Bin2D binX binY
(><) = Bin2D

-- | Get binning algorithm along X axis
binX :: Bin2D bx by -> bx
binX (Bin2D bx _) = bx

-- | Get binning algorithm along Y axis
binY :: Bin2D bx by -> by
binY (Bin2D _ by) = by

instance (Bin binX, Bin binY) => Bin (Bin2D binX binY) where
    type BinValue (Bin2D binX binY) = (BinValue binX, BinValue binY)
    toIndex b@(Bin2D bx by) (x,y) 
        | inRange b (x,y) = toIndex bx x + (toIndex by y)*(fromIntegral $ nBins bx)
        | otherwise       = maxBound
    {-# INLINE toIndex #-}
    fromIndex b@(Bin2D bx by) i = let (ix,iy) = toIndex2D b i
                                  in  (fromIndex bx ix, fromIndex by iy)
    inRange (Bin2D bx by) (x,y) = inRange bx x && inRange by y
    {-# INLINE inRange #-}
    nBins (Bin2D bx by) = (nBins bx) * (nBins by)

toIndex2D :: (Bin binX, Bin binY) => Bin2D binX binY -> Int -> (Int,Int)
toIndex2D b i = let (iy,ix) = divMod i (nBins $ binX b) in (ix,iy)

-- | 2-dimensional size of binning algorithm
nBins2D :: (Bin bx, Bin by) => Bin2D bx by -> (Int,Int)
nBins2D (Bin2D bx by) = (nBins bx, nBins by)

-- | Apply function to X binning algorithm. If new binning algorithm
--   have different number of bins will fail.
fmapBinX :: (Bin bx, Bin bx') => (bx -> bx') -> Bin2D bx by -> Bin2D bx' by
fmapBinX f (Bin2D bx by) 
    | nBins bx' /= nBins bx = error "fmapBinX: new binnig algorithm has different number of bins"
    | otherwise             = Bin2D bx' by
    where 
      bx' = f bx

-- | Apply function to Y binning algorithm. If new binning algorithm
--   have different number of bins will fail.
fmapBinY ::(Bin by, Bin by') => (by -> by') -> Bin2D bx by -> Bin2D bx by'
fmapBinY f (Bin2D bx by)
    | nBins by' /= nBins by = error "fmapBinY: new binnig algorithm has different number of bins"
    | otherwise             = Bin2D bx by'
    where 
      by' = f by

instance (Show b1, Show b2) => Show (Bin2D b1 b2) where
    show (Bin2D b1 b2) = concat [ "# Bin2D\n"
                                , "# X\n"
                                , show b1
                                , "# Y\n"
                                , show b2
                                ]
instance (Read b1, Read b2) => Read (Bin2D b1 b2) where
    readPrec = do
      keyword "Bin2D"
      keyword "X"
      b1 <- readPrec
      keyword "Y"
      b2 <- readPrec
      return $ Bin2D b1 b2


----------------------------------------------------------------
-- Indexed 2D bins
----------------------------------------------------------------
-- | Binning for 2D indexable value
newtype BinIx2D i = BinIx2D {unBinIx2D :: (Bin2D BinI BinI) }

-- | Construct indexed bin
binIx2D :: Indexable2D i => i -> i -> BinIx2D i
binIx2D lo hi = let (ix,iy) = index2D lo
                    (jx,jy) = index2D hi
                in BinIx2D $ BinI ix jx >< BinI iy jy

instance Indexable2D i => Bin (BinIx2D i) where
    type BinValue (BinIx2D i) = i
    toIndex   (BinIx2D b) x = toIndex b (index2D x)
    fromIndex (BinIx2D b) i = deindex2D $ fromIndex b i
    inRange   (BinIx2D b) x = inRange b (index2D x)
    nBins     (BinIx2D b)   = nBins b

instance (Show i, Indexable2D i) => Show (BinIx2D i) where
    show (BinIx2D b) = unlines [ "# BinIx2D"
                               , "# Low  = " ++ show (deindex2D (fromIndex b 0            ) :: i)
                               , "# High = " ++ show (deindex2D (fromIndex b (nBins b - 1)) :: i)
                               ]
instance (Read i, Indexable2D i) => Read (BinIx2D i) where
    readPrec = do
      keyword "BinIx"
      l <- value "Low"
      h <- value "High"
      return $ binIx2D l h
