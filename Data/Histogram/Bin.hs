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
data BinI = BinI (Int,Int)

instance Bin BinI where
    type BinValue BinI = Int
    type BinIndex BinI = Int 
    toIndex   _ = id
    fromIndex _ = id
    getRange (BinI r) = r

