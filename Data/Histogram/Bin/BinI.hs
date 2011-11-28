{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Data.Histogram.Bin.BinI (
    BinI(..)
  , binI
  , binI0
  ) where

import Control.DeepSeq (NFData(..))
import Control.Monad   (liftM2)
import Data.Data       (Data,Typeable)
import Text.Read       (Read(..))

import Data.Histogram.Bin.Classes
import Data.Histogram.Parse



-- | Simple binning algorithm which map continous range of bins onto
-- indices. Each number correcsponds to different bin
--
-- 1. Lower bound (inclusive)
--
-- 2. Upper bound (inclusive)
data BinI = BinI
            {-# UNPACK #-} !Int -- Lower bound (inclusive)
            {-# UNPACK #-} !Int -- Upper bound (inclusive)
            deriving (Eq,Data,Typeable)

-- | Safe constructor for BinI. It does checks that upper bound is
--   greater or equal than lower bound
binI :: Int -> Int -> BinI
binI lo hi | lo <= hi  = BinI lo hi
           | otherwise = error "Data.Histogram.Bin.BinI.binI: invalid paramters"

-- | Construct BinI with n bins. Indexing starts from 0. n must be positive
binI0 :: Int -> BinI
binI0 n = binI 0 (n - 1)

instance Bin BinI where
  type BinValue BinI = Int
  toIndex   !(BinI base _) !x = x - base
  fromIndex !(BinI base _) !x = x + base
  inRange   !(BinI x y) i     = i>=x && i<=y
  nBins     !(BinI x y) = y - x + 1
  {-# INLINE toIndex #-}

instance IntervalBin BinI where
  binInterval b i = (n,n) where n = fromIndex b i

instance Bin1D BinI where
  lowerLimit (BinI i _) = i
  upperLimit (BinI _ i) = i
  unsafeSliceBin i j (BinI l _) = BinI (l+i) (l+j)

instance VariableBin BinI where
  binSizeN _ _ = 1

instance UniformBin BinI where
  binSize _ = 1

instance GrowBin BinI where
  zeroBin    (BinI l _) = BinI l l
  appendBin  (BinI l u) = BinI l (u+1)
  prependBin (BinI l u) = BinI (l-1) u

instance Show BinI where
  show (BinI lo hi) = unlines [ "# BinI"
                              , "# Low  = " ++ show lo
                              , "# High = " ++ show hi
                              ]
instance Read BinI where
  readPrec = keyword "BinI" >> liftM2 BinI (value "Low") (value "High")

instance NFData BinI
