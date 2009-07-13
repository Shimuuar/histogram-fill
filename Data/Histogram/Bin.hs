{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE TypeFamilies     #-}
-- |
-- Module     : Text.Flat
-- Copyright  : Copyright (c) 2009, Alexey Khudyakov <alexey.skladnoy@gmail.com>
-- License    : BSD3
-- Maintainer : Alexey Khudyakov <alexey.skladnoy@gmail.com>
-- Stability  : experimental
-- 
-- Mutable storage for filling histograms. 

module Data.Histogram.Bin ( Bin(..)
                          , BinI(..)
                          , BinF
                          , binF
                          , binFn
                          , Bin2D(..)
                          ) where

import Data.Ix (rangeSize)


-- | Abstract binning algorithm
class Bin b where
    -- | Value of bin 
    type BinValue b
    -- | Convert from value to index. To bound checking performed
    toIndex   :: b -> BinValue b -> Int
    {-# INLINE toIndex #-}
    -- | Convert from index to value. 
    fromIndex :: b -> Int -> BinValue b 
    -- | Range of bin indices (inclusive). 
    getRange  :: b -> (Int, Int)


-- | Integer bins
data BinI = BinI Int Int

instance Bin BinI where
    type BinValue BinI = Int
    toIndex   _ = id
    fromIndex _ = id
    getRange (BinI x y) = (x,y)


-- | Bins with 
data BinF f where
    BinF :: RealFrac f => f -> f -> Int -> BinF f 

binF :: RealFrac f => f -> Int -> f -> BinF f
binF from n to = BinF from ((to - from) / fromIntegral n) n

binFn :: RealFrac f => f -> f -> f -> BinF f 
binFn from step to = BinF from step (round $ (to - from) / step)


instance Bin (BinF f) where
    type BinValue (BinF f) = f 
    toIndex   (BinF from step _) x = floor $ (x-from) / step
    fromIndex (BinF from step _) i = (step/2) + (fromIntegral i * step) + from 
    getRange  (BinF _ _ n) = (0,n-1)
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
