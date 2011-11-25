{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Data.Histogram.Bin.LogBinD (
    -- * Generic and slow
    LogBinD(..)
  , logBinD
  ) where

import Control.Monad (liftM3)
import GHC.Float     (double2Int)
import Data.Typeable (Typeable)
import Data.Data     (Data)
import Text.Read     (Read(..))

import Data.Histogram.Bin.Classes
import Data.Histogram.Parse
-- | Logarithmic scale bins.
--
-- 1. Lower bound
--
-- 2. Increment ratio
--
-- 3. Number of bins
data LogBinD = LogBinD
               Double -- Low border
               Double -- Increment ratio
               Int    -- Number of bins
               deriving (Eq,Data,Typeable)

-- | Create log-scale bins.
logBinD :: Double -> Int -> Double -> LogBinD
logBinD lo n hi = LogBinD lo ((hi/lo) ** (1 / fromIntegral n)) n

-- Fast variant of flooor
floorD :: Double -> Int
floorD x | x < 0     = double2Int x - 1
         | otherwise = double2Int x
{-# INLINE floorD #-}

instance Bin LogBinD where
  type BinValue LogBinD = Double
  toIndex   !(LogBinD base step _) !x = floorD $ logBase step (x / base)
  fromIndex !(LogBinD base step _) !i | i >= 0    = base * step ** (fromIntegral i + 0.5)
                                        | otherwise = -1 / 0
  nBins     !(LogBinD _ _ n) = n
  {-# INLINE toIndex #-}

instance IntervalBin LogBinD where
  binInterval (LogBinD base step _) i = (x, x*step) where x = base * step ** fromIntegral i

instance Bin1D LogBinD where
  lowerLimit (LogBinD lo  _ _) = lo
  upperLimit (LogBinD lo  r n) = lo * r ^ n
  unsafeSliceBin i j (LogBinD from step _) = LogBinD (from * step ^ i) step (j-i+1)

instance VariableBin LogBinD where
  binSizeN (LogBinD base step _) n = let x = base * step ^ n in x*step - x

instance Show LogBinD where
  show b =
    unlines [ "# LogBinD"
            , "# Lo   = " ++ show (lowerLimit b)
            , "# N    = " ++ show (nBins b)
            , "# Hi   = " ++ show (upperLimit b)
            ]
instance Read LogBinD where
  readPrec = do
    keyword "LogBinD"
    liftM3 logBinD (value "Lo") (value "N") (value "Hi")
