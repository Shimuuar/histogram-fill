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
                          ) where


-- | Abstract binning algorithm
class Bin b where
    -- | Value of bin 
    type BinValue b
    -- | Number of bin (should be of Ix type class). 
    -- In future should be simply converted to Int 
    type BinIndex b 
    -- | Convert from value to index. To bound checking performed
    toIndex   :: b -> BinValue b -> BinIndex b
    -- | Convert from index to value. 
    fromIndex :: b -> BinIndex b -> BinValue b 
    -- | Range of bin indices (inclusive). 
    getRange  :: b -> (BinIndex b, BinIndex b)


-- | Integer bins
data BinI = BinI Int Int

instance Bin BinI where
    type BinValue BinI = Int
    type BinIndex BinI = Int 
    toIndex   _ = id
    fromIndex _ = id
    getRange (BinI x y) = (x,y)


-- | 2D bins 
data Bin2D bin1 bin2 = Bin2D bin1 bin2

instance (Bin bin1, Bin bin2) => Bin (Bin2D bin1 bin2) where
    type BinValue (Bin2D bin1 bin2) = (BinValue bin1, BinValue bin2)
    type BinIndex (Bin2D bin1 bin2) = (BinIndex bin1, BinIndex bin2)

    toIndex   (Bin2D b1 b2) (x,y) = (toIndex   b1 x, toIndex   b2 y)
    fromIndex (Bin2D b1 b2) (x,y) = (fromIndex b1 x, fromIndex b2 y)
    getRange  (Bin2D b1 b2) = let (x1,x2) = getRange b1
                                  (y1,y2) = getRange b2
                              in ((x1,y1),(x2,y2))
