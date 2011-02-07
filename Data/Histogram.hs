{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
-- |
-- Module     : Data.Histogram
-- Copyright  : Copyright (c) 2009, Alexey Khudyakov <alexey.skladnoy@gmail.com>
-- License    : BSD3
-- Maintainer : Alexey Khudyakov <alexey.skladnoy@gmail.com>
-- Stability  : experimental
-- 
-- Immutable histograms. 

module Data.Histogram ( -- * Immutable histogram
    -- * Data type
    Histogram
  , module Data.Histogram.Bin
  , histogram
  , histogramUO
    -- * Read histograms from string
  , readHistogram
  , readFileHistogram
    -- * Accessors
  , bins
  , histData
  , underflows
  , overflows
  , outOfRange
    -- ** Convert to other data types
  , asList
  , asVector
    -- * Slicing histograms
  , sliceX
  , sliceY
    -- * Modify histogram
  , histMap
  , histMapBin
  , histZip
  , histZipSafe
  ) where

import qualified Data.Vector.Unboxed    as U
import Data.Vector.Unboxed (Unbox,Vector)

import qualified Data.Histogram.Generic as H
import Data.Histogram.Bin


-- | Immutable histogram. Histogram consists of binning algorithm,
--   optional number of under and overflows, and data. 
type Histogram bin a = H.Histogram U.Vector bin a

-- | Create histogram from binning algorithm and vector with
-- data. Overflows are set to Nothing. 
--
-- Number of bins and vector size must match.
histogram :: (Unbox a, Bin bin) => bin -> Vector a -> Histogram bin a
histogram = H.histogram

-- | Create histogram from binning algorithm and vector with data. 
--
-- Number of bins and vector size must match.
histogramUO :: (Unbox a, Bin bin) => bin -> Maybe (a,a) -> Vector a -> Histogram bin a
histogramUO = H.histogramUO


----------------------------------------------------------------
-- Instances & reading histograms from strings 
----------------------------------------------------------------


-- | Convert String to histogram. Histogram do not have Read instance
--   because of slowness of ReadP
readHistogram :: (Read bin, Read a, Bin bin, Unbox a) => String -> Histogram bin a
readHistogram = H.readHistogram

-- | Read histogram from file.
readFileHistogram :: (Read bin, Read a, Bin bin, Unbox a) => FilePath -> IO (Histogram bin a)
readFileHistogram = H.readFileHistogram

----------------------------------------------------------------
-- Accessors & conversion
----------------------------------------------------------------

-- | Histogram bins
bins :: Histogram bin a -> bin
bins = H.bins

-- | Histogram data as vector
histData :: Histogram bin a -> Vector a
histData = H.histData

-- | Number of underflows
underflows :: Histogram bin a -> Maybe a
underflows = H.underflows

-- | Number of overflows
overflows :: Histogram bin a -> Maybe a
overflows = H.overflows

-- | Underflows and overflows
outOfRange :: Histogram bin a -> Maybe (a,a)
outOfRange = H.outOfRange

-- | Convert histogram to list.
asList :: (Unbox a, Bin bin) => Histogram bin a -> [(BinValue bin, a)]
asList = H.asList

-- | Convert histogram to vector
asVector :: (Bin bin, Unbox a, Unbox (BinValue bin), Unbox (BinValue bin,a)) 
         => Histogram bin a -> Vector (BinValue bin, a) 
asVector = H.asVector

----------------------------------------------------------------
-- Modify histograms
----------------------------------------------------------------

-- | fmap lookalike. It's not possible to create Functor instance
--   because of class restrictions
histMap :: (Unbox a, Unbox b) => (a -> b) -> Histogram bin a -> Histogram bin b
histMap = H.histMap

-- | Apply function to histogram bins. Function must not change number of bins.
--   If it does error is thrown.
histMapBin :: (Bin bin, Bin bin') => (bin -> bin') -> Histogram bin a -> Histogram bin' a
histMapBin = H.histMapBin

-- | Zip two histograms elementwise. Bins of histograms must be equal
--   otherwise error will be called.
histZip :: (Bin bin, Eq bin, Unbox a, Unbox b, Unbox c) =>
           (a -> b -> c) -> Histogram bin a -> Histogram bin b -> Histogram bin c
histZip = H.histZip
           
-- | Zip two histogram elementwise. If bins are not equal return `Nothing`
histZipSafe :: (Bin bin, Eq bin, Unbox a, Unbox b, Unbox c) =>
           (a -> b -> c) -> Histogram bin a -> Histogram bin b -> Maybe (Histogram bin c)
histZipSafe = H.histZipSafe

-- | Slice 2D histogram along Y axis. This function is fast because it does not require reallocations.
sliceY :: (Unbox a, Bin bX, Bin bY) => Histogram (Bin2D bX bY) a -> [(BinValue bY, Histogram bX a)]
sliceY = H.sliceY

-- | Slice 2D histogram along X axis.
sliceX :: (Unbox a, Bin bX, Bin bY) => Histogram (Bin2D bX bY) a -> [(BinValue bX, Histogram bY a)]
sliceX = H.sliceX
