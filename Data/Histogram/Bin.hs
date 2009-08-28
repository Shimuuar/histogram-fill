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
-- Bins for histograms.

module Data.Histogram.Bin ( -- * Type class
                            Bin(..)
                          -- * Integer bins
                          , BinI(..)
                          -- * Floating point bins
                          , BinF
                          , binF
                          , binFn
                          -- * 2D bins
                          , Bin2D(..)
                          ) where

import Data.Ix (rangeSize)


-- | Abstract binning algorithm. Following invariant is expected to hold: 
-- 
-- > toIndex . fromIndex == id
class Bin b where
    -- | Type of value to bin
    type BinValue b
    -- | Convert from value to index. No bound checking performed
    toIndex   :: b -> BinValue b -> Int
    {-# INLINE toIndex #-}
    -- | Convert from index to value. 
    fromIndex :: b -> Int -> BinValue b 
    -- | Range of bin indices (inclusive). 
    getRange  :: b -> (Int, Int)


-- | Integer bins. 
data BinI = BinI !Int !Int

instance Bin BinI where
    type BinValue BinI = Int
    toIndex   _ = id
    fromIndex _ = id
    getRange !(BinI x y) = (x,y)


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
    fromIndex !(BinF from step _) !i = (step/2) + (fromIntegral i * step) + from 
    getRange  !(BinF _ _ n) = (0,n-1)
    {-# SPECIALIZE instance Bin (BinF Double) #-}
    {-# SPECIALIZE instance Bin (BinF Float) #-}


-- | 2D bins 
data Bin2D bin1 bin2 = Bin2D bin1 bin2

instance (Bin bin1, Bin bin2) => Bin (Bin2D bin1 bin2) where
    type BinValue (Bin2D bin1 bin2) = (BinValue bin1, BinValue bin2)

    toIndex   (Bin2D b1 b2) (x,y) = toIndex b1 x + (toIndex b2 y * (rangeSize $ getRange b1))

    fromIndex (Bin2D b1 b2) i = let (ix,iy) = divMod i (rangeSize $ getRange b1)
                                in  (fromIndex b1 ix, fromIndex b2 iy)

    getRange  (Bin2D b1 b2) = let r1 = rangeSize $ getRange b1
                                  r2 = rangeSize $ getRange b2
                              in (0, r1*r2)
