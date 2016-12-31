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
    -- * Immutable histograms
    Histogram
  , module Data.Histogram.Bin
    -- ** Constructors
  , histogram
  , histogramUO
    -- ** Conversion to other data types
  , asList
  , asVector
    -- * Serialization to strings
    -- $serialization
  , readHistogram
  , readFileHistogram
    -- * Accessors
  , bins
  , histData
  , underflows
  , overflows
  , outOfRange
    -- ** Indexing
  , HistIndex(..) 
  , histIndex
  , at
  , atV
  , atI
    -- * Transformations
  , map
  , bmap
  , mapData
  , zip
  , zipSafe
    -- ** Type conversion
  , convertBinning
    -- * Folding
  , foldl
  , bfoldl
    -- ** Specialized folds
  , sum
  , minimum
  , minimumBy
  , maximum
  , maximumBy
  , minIndex
  , minIndexBy
  , maxIndex
  , maxIndexBy
  , minBin
  , minBinBy
  , maxBin
  , maxBinBy
    -- * Slicing & rebinning
  , slice
  , rebin
  , rebinFold
    -- * 2D histograms
    -- ** Slicing
  , sliceAlongX
  , sliceAlongY
  , listSlicesAlongX
  , listSlicesAlongY
    -- ** Reducing along axis
  , reduceX
  , breduceX
  , reduceY
  , breduceY
    -- * Lift histogram transform to 2D
  , liftX
  , liftY
  ) where

import qualified Data.Vector.Unboxed    as U
import Data.Vector.Unboxed (Unbox,Vector)

import qualified Data.Histogram.Generic as H
import Data.Histogram.Generic (HistIndex(..),histIndex)
import Data.Histogram.Bin

import Prelude hiding (map,zip,foldl,sum,maximum,minimum)



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

asVector :: (Bin bin, Unbox a, Unbox (BinValue bin,a)) 
         => Histogram bin a -> Vector (BinValue bin, a) 
asVector = H.asVector

at :: (Bin bin, Unbox a) => Histogram bin a -> HistIndex bin -> a
at = H.at

atV :: (Bin bin, Unbox a) => Histogram bin a -> BinValue bin -> a
atV = H.atV

atI :: (Bin bin, Unbox a) => Histogram bin a -> Int -> a
atI = H.atI



----------------------------------------------------------------
-- Modify histograms
----------------------------------------------------------------

map :: (Unbox a, Unbox b) => (a -> b) -> Histogram bin a -> Histogram bin b
map = H.map

bmap :: (Unbox a, Unbox b, Bin bin)
     => (BinValue bin -> a -> b) -> Histogram bin a -> Histogram bin b
bmap = H.bmap

mapData :: (Unbox a, Unbox b, Bin bin)
        => (Vector a -> Vector b) -> Histogram bin a -> Histogram bin b
mapData = H.mapData


zip :: (BinEq bin, Unbox a, Unbox b, Unbox c) 
    => (a -> b -> c) -> Histogram bin a -> Histogram bin b -> Histogram bin c
zip = H.zip
           
zipSafe :: (BinEq bin, Unbox a, Unbox b, Unbox c)
        => (a -> b -> c) -> Histogram bin a -> Histogram bin b -> Maybe (Histogram bin c)
zipSafe = H.zipSafe

convertBinning :: (ConvertBin bin bin', Unbox a)
               => Histogram bin a -> Histogram bin' a
convertBinning = H.convertBinning


----------------------------------------------------------------
-- Folding
----------------------------------------------------------------

foldl :: (Unbox a) => (b -> a -> b) -> b -> Histogram bin a -> b
foldl = H.foldl

bfoldl :: (Bin bin, Unbox a) => (b -> BinValue bin -> a -> b) -> b -> Histogram bin a -> b
bfoldl = H.bfoldl

sum :: (Unbox a, Num a) => Histogram bin a -> a
sum = H.sum


minimum :: (Unbox a, Ord a) => Histogram bin a -> a
minimum = H.minimum

minimumBy :: (Unbox a) => (a -> a -> Ordering) -> Histogram bin a -> a
minimumBy = H.minimumBy

maximum :: (Unbox a, Ord a) => Histogram bin a -> a
maximum = H.maximum

maximumBy :: (Unbox a) => (a -> a -> Ordering) -> Histogram bin a -> a
maximumBy = H.maximumBy


minIndex :: (Ord a, Unbox a) => Histogram bin a -> Int
minIndex = H.minIndex

minIndexBy :: (Unbox a) => (a -> a -> Ordering) -> Histogram bin a -> Int
minIndexBy = H.minIndexBy

maxIndex :: (Ord a, Unbox a) => Histogram bin a -> Int
maxIndex = H.maxIndex

maxIndexBy :: (Unbox a) => (a -> a -> Ordering) -> Histogram bin a -> Int
maxIndexBy = H.maxIndexBy


minBin :: (Bin bin, Ord a, Unbox a) => Histogram bin a -> BinValue bin
minBin = H.minBin

minBinBy :: (Bin bin, Unbox a) => (a -> a -> Ordering) -> Histogram bin a -> BinValue bin
minBinBy = H.minBinBy

maxBin :: (Bin bin, Ord a, Unbox a) => Histogram bin a -> BinValue bin
maxBin = H.maxBin

maxBinBy :: (Bin bin, Unbox a) => (a -> a -> Ordering) -> Histogram bin a -> BinValue bin
maxBinBy = H.maxBinBy



----------------------------------------------------------------
-- Slicing and reducing histograms
----------------------------------------------------------------

slice :: (SliceableBin bin, Unbox a)
      => HistIndex bin
      -> HistIndex bin
      -> Histogram bin a
      -> Histogram bin a
slice = H.slice

rebin :: (MergeableBin bin, Unbox a)
      => CutDirection
      -> Int      
      -> (a -> a -> a)
      -> Histogram bin a
      -> Histogram bin a
rebin = H.rebin
-- {-# INLINE rebin #-}

rebinFold :: (MergeableBin bin, Unbox a, Unbox b)
          => CutDirection
          -> Int      
          -> (b -> a -> b)
          -> b
          -> Histogram bin a
          -> Histogram bin b
rebinFold = H.rebinFold
-- {-# INLINE rebinFold #-}



----------------------------------------------------------------
-- 2D histograms
----------------------------------------------------------------

sliceAlongX :: (Unbox a, Bin bX, Bin bY)
            => Histogram (Bin2D bX bY) a
            -> HistIndex bY
            -> Histogram bX a
sliceAlongX = H.sliceAlongX

sliceAlongY :: (Unbox a, Bin bX, Bin bY)
            => Histogram (Bin2D bX bY) a
            -> HistIndex bX
            -> Histogram bY a
sliceAlongY = H.sliceAlongY

listSlicesAlongX :: (Unbox a, Bin bX, Bin bY)
                 => Histogram (Bin2D bX bY) a
                 -> [(BinValue bY, Histogram bX a)]
listSlicesAlongX = H.listSlicesAlongX

listSlicesAlongY :: (Unbox a, Bin bX, Bin bY)
                 => Histogram (Bin2D bX bY) a
                 -> [(BinValue bX, Histogram bY a)]
listSlicesAlongY = H.listSlicesAlongY

reduceX :: (Unbox a, Unbox b, Bin bX, Bin bY)
        => (Histogram bX a -> b)
        ->  Histogram (Bin2D bX bY) a
        ->  Histogram bY b
reduceX = H.reduceX

breduceX :: (Unbox a, Unbox b, Bin bX, Bin bY)
         => (BinValue bY -> Histogram bX a -> b)
         ->  Histogram (Bin2D bX bY) a
         ->  Histogram bY b
breduceX = H.breduceX

reduceY :: (Unbox a, Unbox b, Bin bX, Bin bY)
        => (Histogram bY a -> b)
        -> Histogram (Bin2D bX bY) a
        -> Histogram bX b
reduceY = H.reduceY

breduceY :: (Unbox a, Unbox b, Bin bX, Bin bY)
         => (BinValue bX -> Histogram bY a -> b)
         -> Histogram (Bin2D bX bY) a
         -> Histogram bX b
breduceY = H.breduceY

liftX :: (Bin bX, Bin bY, BinEq bX', Unbox a, Unbox b)
      => (Histogram bX a -> Histogram bX' b)
      -> Histogram (Bin2D bX  bY) a
      -> Histogram (Bin2D bX' bY) b
liftX = H.liftX

liftY :: (Bin bX, Bin bY, BinEq bY', Unbox a, Unbox b)
      => (Histogram bY a -> Histogram bY' b)
      -> Histogram (Bin2D bX bY ) a
      -> Histogram (Bin2D bX bY') b
liftY = H.liftY
