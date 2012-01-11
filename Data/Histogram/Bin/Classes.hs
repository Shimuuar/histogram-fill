{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- |
-- Module     : Data.Histogram.Bin
-- Copyright  : Copyright (c) 2011, Alexey Khudyakov <alexey.skladnoy@gmail.com>
-- License    : BSD3
-- Maintainer : Alexey Khudyakov <alexey.skladnoy@gmail.com>
-- Stability  : experimental
--
-- Type classes for binning algorithms. This is mapping from set of
-- interest to integer indices and approximate reverse.
module Data.Histogram.Bin.Classes (
    -- * Bin type class
    Bin(..)
  , binsCenters
    -- * Approximate equality
  , BinEq(..)
    -- * 1D bins
  , IntervalBin(..)
  , Bin1D(..)
  , SliceableBin(..)
  , sliceBin
  , VariableBin(..)
  , UniformBin(..)
    -- * Conversion
  , ConvertBin(..)
  ) where

import qualified Data.Vector.Generic as G
import           Data.Vector.Generic    (Vector)


-- | This type represent some abstract data binning algorithms. It
--   maps sets/intervals of values of type 'BinValue b' to integer
--   indices.
--
--   Following invariant is expected to hold:
--
--   > toIndex . fromIndex == id
class Bin b where
  -- | Type of value to bin
  type BinValue b
  -- | Convert from value to index. Function must not fail for any
  --   input and should produce out of range indices for invalid input.
  toIndex :: b -> BinValue b -> Int
  -- | Convert from index to value. Returned value should correspond
  --   to center of bin. Definition of center is left for definition
  --   of instance. Funtion may fail for invalid indices but
  --   encouraged not to do so.
  fromIndex :: b -> Int -> BinValue b
  -- | Total number of bins.
  nBins :: b -> Int
  -- | Check whether value in range. Have default
  --   implementation. Should satisfy:
  --   inRange b x &#8660; toIndex b x &#8712; [0,nBins b)
  inRange :: b -> BinValue b -> Bool
  inRange b x = i >= 0 && i < nBins b where i = toIndex b x

-- | Return vector of bin centers
binsCenters :: (Bin b, Vector v (BinValue b)) => b -> v (BinValue b)
binsCenters b = G.generate (nBins b) (fromIndex b)
{-# INLINE binsCenters #-}



---- Equality --------------------------------------------------

-- | Approximate equality for bins. It's nessesary to define
--   approximate equality since exact equality is ill defined for bins
--   which work with floating point data. It's not safe to compare
--   floating point numbers for exact equality
class Bin b => BinEq b where
  -- | Approximate equality
  binEq :: b -> b -> Bool



--- 1D bins ----------------------------------------------------

-- | For binning algorithms which work with bin values which have some
--   natural ordering and every bin is continous interval.
class (Bin b, Ord (BinValue b)) => IntervalBin b where
  -- | Interval for n'th bin
  binInterval :: b -> Int -> (BinValue b, BinValue b)
  -- | List of all bins. Could be overridden for efficiency.
  binsList :: Vector v (BinValue b, BinValue b) => b -> v (BinValue b, BinValue b)
  binsList b = G.generate (nBins b) (binInterval b)
  {-# INLINE binsList #-}


-- | 'IntervalBin' which domain is single finite interval
class IntervalBin b => Bin1D b where
  -- | Minimal accepted value of histogram
  lowerLimit :: b -> BinValue b
  -- | Maximal accepted value of histogram
  upperLimit :: b -> BinValue b


-- | Binning algorithm which support slicing.
class Bin b => SliceableBin b where
  -- | Slice bin by indices. This function doesn't perform any checks
  --   and may produce invalid bin. Use 'sliceBin' instead.
  unsafeSliceBin :: Int -> Int -> b -> b

-- | Slice bin using indices
sliceBin :: SliceableBin b => Int -> Int -> b -> b
sliceBin i j b 
  | i < 0  ||  j < 0  ||  i > j  ||  i >= n  ||  j >= n = error "sliceBin: bad slice"
  | otherwise                                           = unsafeSliceBin i j b
    where
      n = nBins b       



---- Bin sizes ------------------------------------------------

-- | 1D binning algorithms with variable bin size
class Bin b => VariableBin b where
  -- | Size of n'th bin.
  binSizeN :: b -> Int -> BinValue b


-- | 1D binning algorithms with constant size bins. Constant sized
--   bins could be thought as specialization of variable-sized bins
--   therefore a superclass constraint.
class VariableBin b => UniformBin b where
  -- | Size of bin. Default implementation just uses 0th bin.
  binSize :: b -> BinValue b
  binSize b = binSizeN b 0



---- Conversion ------------------------------------------------

-- | Class for conversion between binning algorithms.
class (Bin b, Bin b') => ConvertBin b b' where
  -- | Convert bins
  convertBin :: b -> b'
