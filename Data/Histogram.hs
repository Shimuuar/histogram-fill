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
-- Immutable histograms. This module export same APi as
-- 'Data.Histogram.Generic' but specialzed to unboxed vectors. Refer
-- aforementioned module for documentation.
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
    -- * Slicing histogram
  , sliceByIx
  , sliceByVal
    -- * Splitting 2D histograms
  , sliceXatIx
  , sliceYatIx
  , sliceX
  , sliceY
  , reduceX
  , reduceY
    -- * Modify histogram
  , map
  , mapBin
  , zip
  , zipSafe
  ) where

import qualified Data.Vector.Unboxed    as U
import Data.Vector.Unboxed (Unbox,Vector)

import qualified Data.Histogram.Generic as H
import Data.Histogram.Bin

import Prelude hiding (map,zip)



-- | Immutable histogram. Histogram consists of binning algorithm,
--   optional number of under and overflows, and data. 
type Histogram bin a = H.Histogram U.Vector bin a

histogram :: (Unbox a, Bin bin) => bin -> Vector a -> Histogram bin a
histogram = H.histogram

histogramUO :: (Unbox a, Bin bin) => bin -> Maybe (a,a) -> Vector a -> Histogram bin a
histogramUO = H.histogramUO


----------------------------------------------------------------
-- Instances & reading histograms from strings 
----------------------------------------------------------------


readHistogram :: (Read bin, Read a, Bin bin, Unbox a) => String -> Histogram bin a
readHistogram = H.readHistogram

readFileHistogram :: (Read bin, Read a, Bin bin, Unbox a) => FilePath -> IO (Histogram bin a)
readFileHistogram = H.readFileHistogram

----------------------------------------------------------------
-- Accessors & conversion
----------------------------------------------------------------

bins :: Histogram bin a -> bin
bins = H.bins

histData :: Histogram bin a -> Vector a
histData = H.histData

underflows :: Histogram bin a -> Maybe a
underflows = H.underflows

overflows :: Histogram bin a -> Maybe a
overflows = H.overflows

outOfRange :: Histogram bin a -> Maybe (a,a)
outOfRange = H.outOfRange

asList :: (Unbox a, Bin bin) => Histogram bin a -> [(BinValue bin, a)]
asList = H.asList

asVector :: (Bin bin, Unbox a, Unbox (BinValue bin), Unbox (BinValue bin,a)) 
         => Histogram bin a -> Vector (BinValue bin, a) 
asVector = H.asVector

----------------------------------------------------------------
-- Modify histograms
----------------------------------------------------------------

map :: (Unbox a, Unbox b) => (a -> b) -> Histogram bin a -> Histogram bin b
map = H.map

mapBin :: (Bin bin, Bin bin') => (bin -> bin') -> Histogram bin a -> Histogram bin' a
mapBin = H.mapBin

zip :: (Bin bin, Eq bin, Unbox a, Unbox b, Unbox c) =>
           (a -> b -> c) -> Histogram bin a -> Histogram bin b -> Histogram bin c
zip = H.zip
           
zipSafe :: (Bin bin, Eq bin, Unbox a, Unbox b, Unbox c) =>
           (a -> b -> c) -> Histogram bin a -> Histogram bin b -> Maybe (Histogram bin c)
zipSafe = H.zipSafe

sliceByIx :: (Bin1D bin, Unbox a) => Int -> Int -> Histogram bin a -> Histogram bin a
sliceByIx = H.sliceByIx

sliceByVal :: (Bin1D bin, Unbox a) => BinValue bin -> BinValue bin -> Histogram bin a -> Histogram bin a
sliceByVal = H.sliceByVal


sliceXatIx :: (Unbox a, Bin bX, Bin bY)
           => Histogram (Bin2D bX bY) a
           -> Int
           -> Histogram bX a
sliceXatIx = H.sliceXatIx

sliceYatIx :: (Unbox a, Bin bX, Bin bY)
           => Histogram (Bin2D bX bY) a
           -> Int
           -> Histogram bY a
sliceYatIx = H.sliceYatIx


sliceY :: (Unbox a, Bin bX, Bin bY) => Histogram (Bin2D bX bY) a -> [(BinValue bY, Histogram bX a)]
sliceY = H.sliceY

sliceX :: (Unbox a, Bin bX, Bin bY) => Histogram (Bin2D bX bY) a -> [(BinValue bX, Histogram bY a)]
sliceX = H.sliceX

reduceX :: (Unbox a, Unbox b, Bin bX, Bin bY)
        => Histogram (Bin2D bX bY) a -> (Histogram bX a -> b) -> Histogram bY b
reduceX = H.reduceX

reduceY :: (Unbox a, Unbox b, Bin bX, Bin bY)
        => Histogram (Bin2D bX bY) a -> (Histogram bY a -> b) -> Histogram bX b
reduceY = H.reduceY
