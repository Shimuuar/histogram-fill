{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Data.Histogram.Bin.BinInt (
    BinInt(..)
  , binInt
  , binIntN
  ) where

import Control.Monad (liftM3)
import Data.Typeable (Typeable)
import Data.Data     (Data)
import Text.Read     (Read(..))

import Data.Histogram.Bin.Classes
import Data.Histogram.Parse



-- | Integer bins with size which differ from 1.
--
-- 1. Low bound
--
-- 2. Bin size
--
-- 3. Number of bins
data BinInt = BinInt
              {-# UNPACK #-} !Int -- Low bound
              {-# UNPACK #-} !Int -- Bin size
              {-# UNPACK #-} !Int -- Number of bins
              deriving (Eq,Data,Typeable)

-- FIXME: no sanity checks
-- | Construct BinInt.
binInt :: Int                   -- ^ Lower bound
       -> Int                   -- ^ Bin size
       -> Int                   -- ^ Upper bound
       -> BinInt
binInt lo n hi = BinInt lo n nb
  where
    nb = (hi-lo) `div` n

binIntN :: Int                  -- ^ Lower bound
        -> Int                  -- ^ Bin size
        -> Int                  -- ^ Upper bound
        -> BinInt
binIntN lo n hi 
  | n > rng   = BinInt lo 1 rng
  | otherwise = BinInt lo undefined n
  where
    rng = hi - lo + 1


instance Bin BinInt where
  type BinValue BinInt = Int
  toIndex   !(BinInt base sz _) !x = (x - base) `div` sz
  fromIndex !(BinInt base sz _) !x = x * sz + base
  nBins     !(BinInt _ _ n) = n
  {-# INLINE toIndex #-}

instance IntervalBin BinInt where
  binInterval b i = (n, n + binSize b - 1) where n = fromIndex b i

instance Bin1D BinInt where
  lowerLimit (BinInt base _  _) = base
  upperLimit (BinInt base sz n) = base + sz * n - 1
  unsafeSliceBin i j (BinInt base sz _) = BinInt (base + i*sz) sz (j-i+1)

instance GrowBin BinInt where
  zeroBin    (BinInt l sz _) = BinInt l sz 0
  appendBin  (BinInt l sz n) = BinInt l sz (n+1)
  prependBin (BinInt l sz n) = BinInt (l-sz) sz (n+1)

instance VariableBin BinInt where
  binSizeN (BinInt _ sz _) _ = sz

instance UniformBin BinInt where
  binSize (BinInt _ sz _) = sz

instance Show BinInt where
  show (BinInt base sz n) =
    unlines [ "# BinInt"
            , "# Base = " ++ show base
            , "# Step = " ++ show sz
            , "# Bins = " ++ show n
            ]

instance Read BinInt where
  readPrec = keyword "BinInt" >> liftM3 BinInt (value "Base") (value "Step") (value "Bins")
