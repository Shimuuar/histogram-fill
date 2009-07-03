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


class Bin b where
    type BinValue b
    type BinIndex b 
    toIndex   :: b -> BinValue b -> BinIndex b
    fromIndex :: b -> BinIndex b -> BinValue b 
    getRange  :: b -> (BinIndex b, BinIndex b)

-- Integer bins
data BinI = BinI (Int,Int)

instance Bin BinI where
    type BinValue BinI = Int
    type BinIndex BinI = Int 
    toIndex   _ = id
    fromIndex _ = id
    getRange (BinI r) = r

