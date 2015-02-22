{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE TypeSynonymInstances   #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE MultiWayIf #-}

-- Immutable histogram specialised to Unboxed Double, that can add and delete bins
module Data.Histogram.Adaptable (
    -- * Immutable adaptable histograms
    HistogramDU
    , sliceAt
    , insertAt
    , mergeAtCut
    , smallestCutContiguous
    , smallestCutSingle
    , mergeSmallest
    , mergeSmallestSingle
  ) where

import qualified Data.Vector.Unboxed as V
import Data.Vector.Unboxed ((!),(++))

import Data.Histogram
import Data.Histogram.Bin.BinDU

import Prelude hiding ((++))

-- | Immutable Adaptable histogram.
type HistogramDU = Histogram BinDU Double

sliceAt :: Histogram BinDU Double -> Double -> Histogram BinDU Double
sliceAt h x
    | V.length (cuts (bins h)) == 0 = addFirst
    | x < lowerLimit (bins h) = addLower
    | x > upperLimit (bins h) = addUpper
    | otherwise = addMiddle
  where
    b = bins h
    n = nBins b
    b' = addCut b x
    v = histData h
    i = toIndex b x
    freq = h `atV` x
    r = binInterval b i
    size = binSizeN b i
    slice0 = freq * (x - fst r)/size
    slice1 = freq * (snd r - x)/size
    addFirst  = histogram (unsafeBinDU (V.fromList [x])) V.empty
    addLower  = histogram b' (V.singleton 0 ++ v)
    addUpper  = histogram b' (v ++ V.singleton 0)
    addMiddle = histogram b' (V.concat
                              [ V.take i v
                              , V.fromList [slice0,slice1]
                              , V.drop (min (i+1) (n+1)) v
                              ])


-- | dont interpolate the bin values
insertAt :: Histogram BinDU Double -> Double -> Histogram BinDU Double
insertAt h x
    | V.length (cuts (bins h)) == 0 = addFirst
    | x < lowerLimit (bins h) = addLower
    | x > upperLimit (bins h) = addUpper
    | otherwise = addMiddle
  where
    b = bins h
    n = nBins b
    b' = addCut b x
    v = histData h
    i = toIndex b x
    addFirst  = histogram (unsafeBinDU (V.fromList [x])) V.empty
    addLower  = histogram b' (V.singleton 0 ++ v)
    addUpper  = histogram b' (v ++ V.singleton 0)
    addMiddle = histogram b' (V.concat
                              [ V.take (i+1) v
                              , V.singleton 0
                              , V.drop (min (i+1) (n+1)) v
                              ])

mergeAtCut :: HistogramDU -> Int -> HistogramDU
mergeAtCut h i
       | i <0 || i>n = error "Data.Histogram.HistogramA': outside index range"
       | i == 0 = case h `atI` 0 of
             0 -> histogram (deleteCut (bins h) i) (V.drop 1 v) 
             _ -> error "Data.Histogram.HistogramA': can't delete outer bin with non-zero frequency"
       | i == n = case h `atI` (i-1) of
             0 -> histogram (deleteCut (bins h) i) (V.init v) 
             _ -> error "Data.Histogram.HistogramA': can't delete outer bin with non-zero frequency"
       | otherwise = histogram b' v'
  where
    n  = nBins (bins h)
    b' = deleteCut (bins h) i
    v  = histData h
    v' = V.concat
         [ V.take (max 0 (i-1)) v
         , V.singleton (v ! (i - 1) + v ! i)
         , V.drop (min (i+1) n) v
         ]

smallestCutContiguous :: (Bin b, V.Unbox a, Ord a, Num a) => Histogram b a -> Int
smallestCutContiguous h
    | v ! 0 == 0 = 0
    | v ! n == 0 = n+1
    | otherwise = 1 + V.minIndex (V.zipWith (+) (V.init v) (V.tail v))
  where
    v = histData h
    n = nBins (bins h) - 1

smallestCutSingle :: (Bin b, V.Unbox a, Ord a, Num a) => Histogram b a -> Int
smallestCutSingle h
    | v ! 0 == 0 = 0
    | v ! n == 0 = n+1
    | mini == 0 = 1
    | mini == n = n
    | v!(mini-1) < v!(mini+1) = mini
    | otherwise = mini+1
  where
    v = histData h
    n = nBins (bins h) - 1
    mini = V.minIndex v

mergeSmallest :: HistogramDU -> HistogramDU
mergeSmallest h = mergeAtCut h (smallestCutContiguous h)

mergeSmallestSingle :: HistogramDU -> HistogramDU
mergeSmallestSingle h = mergeAtCut h (smallestCutSingle h)
