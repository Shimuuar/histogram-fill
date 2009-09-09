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
                          -- * 2D bins
                          , Bin2D(..)
                          ) where


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
    {-# INLINE toIndex #-}
    -- | Convert from index to value. 
    fromIndex :: b -> Int -> BinValue b 
    -- | Total number of bins
    nBins :: b -> Int


-- | Integer bins. This is inclusive interval [from,to]
data BinI = BinI !Int !Int

instance Bin BinI where
    type BinValue BinI = Int
    toIndex   !(BinI base _) !x = x - base
    fromIndex !(BinI base _) !x = x + base
    nBins     !(BinI x y) = y - x + 1


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
    nBins     !(BinF _ _ n) = n
    {-# SPECIALIZE instance Bin (BinF Double) #-}
    {-# SPECIALIZE instance Bin (BinF Float) #-}


-- | 2D bins. bin1 is binning along X axis and bin2 is one along Y axis. 
data Bin2D bin1 bin2 = Bin2D bin1 bin2

instance (Bin bin1, Bin bin2) => Bin (Bin2D bin1 bin2) where
    type BinValue (Bin2D bin1 bin2) = (BinValue bin1, BinValue bin2)

    toIndex   (Bin2D bx by) (x,y) 
        | ix < 0 || ix >= rx || iy < 0 || iy >= ry = maxBound
        | otherwise                                = ix + iy*rx
        where
          ix = toIndex bx x
          iy = toIndex by y
          rx = nBins bx
          ry = nBins by

    fromIndex (Bin2D bx by) i = let (iy,ix) = divMod i (nBins bx)
                                in  (fromIndex bx ix, fromIndex by iy)

    nBins (Bin2D b1 b2) = (nBins b1) * (nBins b2)
