-- Requred for Bin2D conversions
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverlappingInstances #-}
-- Yes I DO want orphans here
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- |
-- Module     : Data.Histogram.Bin
-- Copyright  : Copyright (c) 2009, Alexey Khudyakov <alexey.skladnoy@gmail.com>
-- License    : BSD3
-- Maintainer : Alexey Khudyakov <alexey.skladnoy@gmail.com>
-- Stability  : experimental
--
-- Binning algorithms. This is mapping from set of interest to integer
-- indices and approximate reverse.

module Data.Histogram.Bin ( 
    -- * Type classes
    module Data.Histogram.Bin.Classes
  , module Data.Histogram.Bin.BinI
  , module Data.Histogram.Bin.BinInt
  , module Data.Histogram.Bin.BinEnum
  , module Data.Histogram.Bin.BinF
  , module Data.Histogram.Bin.LogBinD
  , module Data.Histogram.Bin.Bin2D
  ) where

import Data.Histogram.Bin.Classes
import Data.Histogram.Bin.BinI
import Data.Histogram.Bin.BinInt
import Data.Histogram.Bin.BinEnum
import Data.Histogram.Bin.BinF
import Data.Histogram.Bin.LogBinD
import Data.Histogram.Bin.Bin2D

----------------------------------------------------------------
-- Bin conversion
----------------------------------------------------------------

-- BinI -> BinInt
instance ConvertBin BinI BinInt where
  convertBin b = binIntN (lowerLimit b) 1 (upperLimit b)

-- BinI,BinInt -> BinF
instance RealFrac f => ConvertBin BinI (BinF f) where
  convertBin b = binFstep (fromIntegral (lowerLimit b) - 0.5) 1 (nBins b)
instance RealFrac f => ConvertBin BinInt (BinF f) where
  convertBin b = binFstep (fromIntegral (lowerLimit b) - 0.5) (fromIntegral $ binSize b) (nBins b)

-- BinI,BinInt -> BinD
instance ConvertBin BinI BinD where
  convertBin b = binDstep (fromIntegral (lowerLimit b) - 0.5) 1 (nBins b)
instance ConvertBin BinInt BinD where
  convertBin b = binDstep (fromIntegral (lowerLimit b) - 0.5) (fromIntegral $ binSize b) (nBins b)

-- Bin2D -> Bin2D
instance (ConvertBin bx bx', Bin by) => ConvertBin (Bin2D bx by) (Bin2D bx' by) where
  convertBin = fmapBinX convertBin
instance (ConvertBin by by', Bin bx) => ConvertBin (Bin2D bx by) (Bin2D bx by') where
  convertBin = fmapBinY convertBin
instance (ConvertBin bx bx', ConvertBin by by') => ConvertBin (Bin2D bx by) (Bin2D bx' by') where
  convertBin (Bin2D bx by) = Bin2D (convertBin bx) (convertBin by)
