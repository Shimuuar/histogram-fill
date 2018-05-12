-- |
-- Module     : Data.Histogram.Tutorial
-- Copyright  : Copyright (c) 2009-2018, Alexey Khudyakov <alexey.skladnoy@gmail.com>
-- License    : BSD3
-- Maintainer : Alexey Khudyakov <alexey.skladnoy@gmail.com>
-- Stability  : experimental
--
--
-- == 1.
--
-- The first example illustrates one of the most common use-cases of a histogram, i.e.
--
-- * uniform binning
--
-- * one-dimensional distribuded data
--
-- * binning range equal to the data range
-- 
-- Here we populate a 'Histogram' from a 'Foldable' container (e.g. an array, or a 'Vector', or a tree, etc.) of 'Double's :
-- 
-- @
-- histo :: ('Foldable' v, 'Unbox' a, Num a) =>
--          Int
--       -> v Double
--       -> 'Histogram' 'BinD' a
-- histo n v = 'fillBuilder' buildr v
--   where
--     mi = minimum v
--     ma = maximum v
--     bins = 'binD' mi n ma
--     buildr = 'mkSimple' bins
-- @
module Data.Histogram.Tutorial where

import Data.Histogram 
import Data.Histogram.Bin
import Data.Histogram.Fill (mkSimple, fillBuilder)
import Data.Vector.Unboxed (Unbox(..))
