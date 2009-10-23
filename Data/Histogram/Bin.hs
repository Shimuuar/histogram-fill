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

module Data.Histogram.Bin ( -- * Type class
                            Bin(..)
                          -- * Integer bins
                          , BinI(..)
                          -- * Floating point bins
                          , BinF
                          , binF
                          , binFn
                          , binI2binF
                          , scaleBinF
                          -- * 2D bins
                          , Bin2D(..)
                          , (><)
                          , fmapBinX
                          , fmapBinY
                          ) where

import Data.Histogram.Parse
import Text.Read (Read(..))



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
-- Integer bin

-- | Integer bins. This is inclusive interval [from,to]
data BinI = BinI !Int !Int

instance Bin BinI where
    type BinValue BinI = Int
    toIndex   !(BinI base _) !x = x - base
    {-# INLINE toIndex #-}
    fromIndex !(BinI base _) !x = x + base
    inRange   !(BinI x y) i     = i>=x && i<=y
    {-# INLINE inRange #-}
    nBins     !(BinI x y) = y - x + 1

instance Show BinI where
    show (BinI lo hi) = unlines [ "# BinI"
                                , "# Low  = " ++ show lo
                                , "# High = " ++ show hi
                                ]

instance Read BinI where
    readPrec = do
      keyword "BinI"
      l <- value "Low"
      h <- value "High"
      return $ BinI l h


----------------------------------------------------------------
-- Floating point bin

-- | Floaintg point bins with equal sizes.
data BinF f where
    BinF :: RealFrac f => !f -> !f -> !Int -> BinF f 

-- | Create bins 
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

instance Bin (BinF f) where
    type BinValue (BinF f) = f 
    toIndex   !(BinF from step _) !x = floor $ (x-from) / step
    {-# INLINE toIndex #-}
    fromIndex !(BinF from step _) !i = (step/2) + (fromIntegral i * step) + from 
    inRange   !(BinF from step n) x  = x > from && x < from + step*fromIntegral n
    {-# INLINE inRange #-}
    nBins     !(BinF _ _ n) = n
    {-# SPECIALIZE instance Bin (BinF Double) #-}
    {-# SPECIALIZE instance Bin (BinF Float) #-}

-- | Convert BinI to BinF
binI2binF :: RealFrac f => BinI -> BinF f
binI2binF b@(BinI i _) = BinF (fromIntegral i) 1 (nBins b)

-- | 'scaleBinF a b' scales BinF using linaer transform 'a+b*x'
scaleBinF :: RealFrac f => f -> f -> BinF f -> BinF f
scaleBinF a b (BinF base step n) 
    | b > 0     = BinF (a + b*base) (b*step) n
    | otherwise = error "scaleBinF: b must be positive"

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
-- 2D bin

-- | 2D bins. binX is binning along X axis and binY is one along Y axis. 
data Bin2D binX binY = Bin2D binX binY

-- | Alias for 'Bin2D'.
(><) :: binX -> binY -> Bin2D binX binY
(><) = Bin2D

instance (Bin binX, Bin binY) => Bin (Bin2D binX binY) where
    type BinValue (Bin2D binX binY) = (BinValue binX, BinValue binY)

    toIndex b@(Bin2D bx by) (x,y) 
        | inRange b (x,y) = toIndex bx x + (toIndex by y)*(fromIntegral $ nBins bx)
        | otherwise       = maxBound
    {-# INLINE toIndex #-}
    fromIndex (Bin2D bx by) i = let (iy,ix) = divMod i (nBins bx)
                                in  (fromIndex bx ix, fromIndex by iy)
    inRange (Bin2D bx by) (x,y) = inRange bx x && inRange by y
    {-# INLINE inRange #-}

    nBins (Bin2D bx by) = (nBins bx) * (nBins by)

-- | 2-dimensional size of 
nBins2D :: (Bin bx, Bin by) => Bin2D bx by -> (Int,Int)
nBins2D (Bin2D bx by) = (nBins bx, nBins by)

-- | Apply function to X binning algorithm. 
--
-- N.B. This is dangerous function. If new binning algorithm doesn't
-- have the same numer of bins it could lead to diffcult to find
-- errors.
fmapBinX :: (bx -> bx') -> Bin2D bx by -> Bin2D bx' by
fmapBinX f (Bin2D bx by) = Bin2D (f bx) by

-- | Apply function to Y binning algorithm.
--
-- N.B. This is dangerous function. If new binning algorithm doesn't
-- have the same numer of bins it could lead to diffcult to find
-- errors.
fmapBinY :: (by -> by') -> Bin2D bx by -> Bin2D bx by'
fmapBinY f (Bin2D bx by) = Bin2D bx (f by)

instance (Show b1, Show b2) => Show (Bin2D b1 b2) where
    show (Bin2D b1 b2) = "# Bin2D\n" ++
                         "# X\n" ++ 
                         show b1 ++
                         "# Y\n" ++
                         show b2
instance (Read b1, Read b2) => Read (Bin2D b1 b2) where
    readPrec = do
      keyword "Bin2D"
      keyword "X"
      b1 <- readPrec
      keyword "Y"
      b2 <- readPrec
      return $ Bin2D b1 b2
