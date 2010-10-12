{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE DeriveDataTypeable  #-}
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
                          -- * Bin types
                          -- ** Integer bins
                          , BinI(..)
                          , binI0
                          -- ** Integer bins with non-1 size
                          , BinInt(..)
                          , binInt
                          -- ** Floating point bins
                          , BinF
                          , binF
                          , binFn
                          , binI2binF
                          , scaleBinF
                          -- *** Specialized for Double 
                          , BinD(..)
                          , binD
                          , binDn
                          , binI2binD
                          , scaleBinD
                          -- ** Log scale point
                          , LogBinD(..)
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
                          ) where

import Control.Monad (liftM2, liftM3)
import GHC.Float     (double2Int)

import qualified Data.Vector.Generic as G
import Data.Typeable                    (Typeable)
import Text.Read                        (Read(..))

import Data.Histogram.Parse



----------------------------------------------------------------
-- | Abstract binning algorithm. It provides way to map some values
-- onto continous range of integer values starting from zero. 
-- 
-- Following invariant is expected to hold: 
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
-- | One dimensional binning algorithm. It means that bin values have
-- some inherent ordering. For example all binning algorithms for real
-- numbers could be members or this type class whereas binning
-- algorithms for R^2 could not. 
class Bin b => Bin1D b where
    -- | Size of i'th bin.
    binSize :: b -> Int -> BinValue b
    -- | List of center of bins in ascending order.
    binsList :: G.Vector v (BinValue b) => b -> v (BinValue b)
    -- | List of bins in ascending order. First element of tuple is
    --   lower bound second is upper bound of bin
    binsListRange :: G.Vector v (BinValue b, BinValue b) => b -> v (BinValue b, BinValue b)

----------------------------------------------------------------
-- Integer bin
----------------------------------------------------------------
-- | Simple binning algorithm which map continous range of bins onto
-- indices. Each number correcsponds to different bin
data BinI = BinI {-# UNPACK #-} !Int {-# UNPACK #-} !Int
            deriving (Eq,Typeable)

-- | Construct BinI with n bins. Indexing starts from 0
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
    binSize _ _ = 1
    binsList      b@(BinI lo _) = G.enumFromN lo (nBins b)
    binsListRange b@(BinI lo _) = G.generate (nBins b) (\i -> (lo+i, lo+i))

instance Show BinI where
    show (BinI lo hi) = unlines [ "# BinI"
                                , "# Low  = " ++ show lo
                                , "# High = " ++ show hi
                                ]
instance Read BinI where
    readPrec = keyword "BinI" >> liftM2 BinI (value "Low") (value "High")

----------------------------------------------------------------
-- Another form of Integer bin
----------------------------------------------------------------

-- | Integer bins with size which differ from 1.
data BinInt = BinInt 
              {-# UNPACK #-} !Int -- ^ Low bound
              {-# UNPACK #-} !Int -- ^ Bin size
              {-# UNPACK #-} !Int -- ^ Number of bins
              deriving (Eq,Typeable)

-- | Construct BinInt.
binInt :: Int                   -- ^ Lower bound
       -> Int                   -- ^ Bin size
       -> Int                   -- ^ Upper bound
       -> BinInt
binInt lo n hi = BinInt lo n nb
  where
    nb = (hi-lo) `div` n 

instance Bin BinInt where
    type BinValue BinInt = Int
    toIndex   !(BinInt base sz _) !x = (x - base) `div` sz
    {-# INLINE toIndex #-}
    fromIndex !(BinInt base sz _) !x = x * sz + base
    inRange   !(BinInt base sz n) i  = i>=base && i<(base+n*sz)
    {-# INLINE inRange #-}
    nBins     !(BinInt _ _ n) = n

instance Show BinInt where
    show (BinInt base sz n) = 
      unlines [ "# BinInt"
              , "# Base = " ++ show base
              , "# Step = " ++ show sz
              , "# Bins = " ++ show n
              ]

instance Read BinInt where
    readPrec = keyword "BinInt" >> liftM3 BinInt (value "Base") (value "Step") (value "Bins")


----------------------------------------------------------------
-- Floating point bin
----------------------------------------------------------------
-- | Floaintg point bins with equal sizes.
data BinF f where
    BinF :: RealFrac f => !f -> !f -> !Int -> BinF f 
    deriving Typeable

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
    binSize (BinF _ step _) _ = step
    binsList      b@(BinF _    _ n) = G.generate n (fromIndex b)
    binsListRange b@(BinF _ step n) = G.generate n toPair
        where
          toPair k = (x - step/2, x + step/2) where x = fromIndex b k

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
-- | Floaintg point bins with equal sizes. If you work with Doubles
-- this data type should be used instead of BinF.
data BinD = BinD {-# UNPACK #-} !Double {-# UNPACK #-} !Double {-# UNPACK #-} !Int
            deriving Typeable

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
    binSize (BinD _ step _) _ = step
    binsList      b@(BinD _    _ n) = G.generate n (fromIndex b)
    binsListRange b@(BinD _ step n) = G.generate n toPair
        where
          toPair k = (x - step/2, x + step/2) where x = fromIndex b k


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
-- | Logarithmic scale bins.
data LogBinD = LogBinD
               Double -- ^ Low border
               Double -- ^ Hi border
               Double -- ^ Increment ratio
               Int    -- ^ Number of bins
               deriving (Eq,Typeable)

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
data Bin2D binX binY = Bin2D !binX !binY
                       deriving (Eq,Typeable)

-- | Alias for 'Bin2D'.
(><) :: binX -> binY -> Bin2D binX binY
(><) = Bin2D

-- | Get binning algorithm along X axis
binX :: Bin2D bx by -> bx
binX !(Bin2D bx _) = bx

-- | Get binning algorithm along Y axis
binY :: Bin2D bx by -> by
binY !(Bin2D _ by) = by

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
