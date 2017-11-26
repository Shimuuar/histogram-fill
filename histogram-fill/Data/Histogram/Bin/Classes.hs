{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
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
    -- ** Common index data types
  , IndexUO(..)
  , IndexFloatUO(..)
    -- ** Ranges
  , Range1D(..)
  , OrderedDomain(..)
  , IsIEEE754(..)
  -- , Interval(..)
  -- , Point(..)
    -- * Other bin type classes
  , Bin1D(..)
  , UniformBin(..)
    -- * Rebinning
  , Rebinning(..)
  , SliceableBin(..)
  , DropAt(..)
  , MergeableBin(..)
    -- * Conversion
  , ConvertBin(..)
  ) where

import Data.Data (Typeable,Data)
import qualified Data.Vector.Generic as G
import           Data.Vector.Generic    (Vector)
import GHC.Generics (Generic)

----------------------------------------------------------------
-- Binning algorithm
----------------------------------------------------------------

-- | This type represent some abstract binning algorithms. It
--   maps sets/intervals of values of type 'BinValue b' to integer
--   indices.
--
--   Instance should satisfy following laws:
--
--   > 1. Totality of indexing
--   > ∀x                       . isIndexValid b (toIndex b x) == True
--   > {∀i :  isIndexValid b i} . 0 <= toBufferIdx b i < totalBinNumber b
--   >
--   > 2. Partial invertibility
--   >
--   >
class Bin b where
  -- | Type of value to bin
  type BinValue b
  -- | Index domain for bins.
  type BinIdx b
  -- | Set of values from bin domain corresponding to the single bin
  --   index
  type BinValueSet b -- FIXME: naming

  -- | Convert from value domain to index domain. This conversion must
  --   be total.
  toIndex        :: b -> BinValue b -> BinIdx b
  -- | Set of domain values corresponding to given index.
  fromIndex      :: b -> BinIdx b -> BinValueSet b
  -- | Check if index is valid
  isIndexValid   :: b -> BinIdx b -> Bool

  -- | Convert index to buffer index.
  --
  --   WARNING: this function should only be called with indexes such that:
  --
  -- > isIndexValid b idx == True
  toBufferIdx    :: b -> BinIdx b -> Int
  -- | Convert integer index in the linear buffer to index
  fromBufferIdx  :: b -> Int -> BinIdx b
  -- | Total bin number including overflow/underflow bins (if they exist)
  totalBinNumber :: b -> Int


----------------------------------------------------------------
-- Common index data types
----------------------------------------------------------------

-- | Index for bins with overflow bins
data IndexUO
  = IdxU                -- ^ Underflow bin
  | IdxN !Int           -- ^ Normal bin
  | IdxO                -- ^ Overflow bin
  deriving (Show,Eq,Ord)

-- | Index for bins with overflow bins and special case for
--   NaNs. Should be used for IEEE754 numbers
data IndexFloatUO
  = FIdxU                       -- ^ Underflow bin
  | FIdxN !Int                  -- ^ Normal bin
  | FIdxO                       -- ^ Overflow bin
  | FIdxNaN                     -- ^ Bin for NaNs
  deriving (Show,Eq,Ord)



----------------------------------------------------------------
-- Common intervals data types
----------------------------------------------------------------

-- | Data type for ordered domains
data OrderedDomain
  = DomEnumeration
  | DomInterval IsIEEE754

data IsIEEE754
  = NormalNumber
  | IEEE754Number

data Range1D dom a where
  -- | @LessThan a@ corresponds to \[ x < a \]
  LessThan :: !a -> Range1D dom a
  -- | @GEqThan a@  corresponds to \[ x \geq a \]
  GEqThan  :: !a -> Range1D dom a
  --
  Point :: !a -> Range1D 'DomEnumeration a
  --
  Interval :: !a -> !a -> Range1D ('DomInterval num) a
  -- |
  NaNValue   :: Range1D ('DomInterval 'IEEE754Number) a


-- -- | Interval on
-- data Range1D f a
--   = LessThan    !a
--   | Finite      !(f a)
--   | GreaterThan !a
--   -- deriving (Show,Eq,Ord)

-- data Interval a = Interval a a

-- newtype Point a = Point a



----------------------------------------------------------------
-- Various rebinning algorithms
----------------------------------------------------------------

class Bin bin => Bin1D bin where
  -- | Minimal accepted value of histogram
  lowerLimit :: bin -> BinValue bin
  -- | Maximal accepted value of histogram
  upperLimit :: bin -> BinValue bin

-- | 1D binning algorithms with constant size bins. Constant sized
--   bins could be thought as specialization of variable-sized bins
--   therefore a superclass constraint.
class Bin bin => UniformBin bin where
  -- | Size of bin. Default implementation just uses 0th bin.
  binSize :: bin -> BinValue bin


----------------------------------------------------------------
-- Rebinnings
----------------------------------------------------------------

data Rebinning b b' = Rebinning
  { rebinning :: b -> Maybe b'
    -- ^ Mapping transformation of bin to binning
  , sourceBins :: BinIdx b' -> [BinIdx b]
    -- ^ Set of older bins which correspond to single new bin
  }

-- | Binning algorithm which support slicing.
class Bin bin => SliceableBin bin where
  -- | Slice bin by indices. This function doesn't perform any checks
  --   and may produce invalid bin. Use 'sliceBin' instead.
  sliceBin :: Int         -- ^ Index of first bin (zero based)
           -> Int         -- ^ Index of last bin
           -> Rebinning bin bin

-- | How index should be dropped
data DropAt = DropLower    -- ^ Drop bins with smallest index
            | DropHigher   -- ^ Drop bins with bigger index
            deriving (Show,Typeable,Data,Generic)

-- | Bin which support rebinning.
class Bin bin => MergeableBin bin where
  -- | @N@ consecutive bins are joined into single bin. If number of
  --   bins isn't multiple of @N@ remaining bins with highest or
  --   lowest index are dropped. This function doesn't do any
  --   checks. Use 'mergeBins' instead.
  mergeBins :: DropAt -> Int -> Rebinning bin bin


---- Conversion ------------------------------------------------

-- | Class for conversion between binning algorithms.
class (Bin b, Bin b') => ConvertBin b b' where
  -- | Convert bins
  convertBin :: b -> b'
