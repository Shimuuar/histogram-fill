{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Data.Histogram.Bin.LogBinD (
    LogBinD
  , logBinDIncrement
  , logBinD
  , logBinDN
  ) where

import Control.DeepSeq (NFData(..))
import Control.Monad   (liftM3)
import GHC.Float       (double2Int)
import Data.Data       (Data,Typeable)
import Text.Read       (Read(..))

import Data.Histogram.Bin.Classes
import Data.Histogram.Bin.Read



-- | Uniform binning in logarithmic scale. For roundtripping use:
--
-- > b = logBinDN (lowerLimit b) (logBinDIncrement b) (nBins b)
data LogBinD = LogBinD
               Double -- Low border
               Double -- Increment ratio
               Int    -- Number of bins
               deriving (Eq,Data,Typeable)

-- | Increment ratio for 'LogBinD'
logBinDIncrement :: LogBinD -> Double
logBinDIncrement (LogBinD _ x _) = x
  
-- | Create log-scale binning algorithm.
logBinD :: Double               -- ^ Lower limit
        -> Int                  -- ^ Number of bins
        -> Double               -- ^ Upper limit
        -> LogBinD
logBinD lo n hi 
  | lo * hi <= 0  = error "Data.Histogram.Bin.LogBinD.logBinD: interval must not inlude zero"
  | n < 0         = error "Data.Histogram.Bin.LogBinD.logBinD: negative number of bins"
  | otherwise     = LogBinD lo ((hi/lo) ** (1 / fromIntegral n)) n

logBinDN :: Double              -- ^ Lower limit
         -> Double              -- ^ Increment ratio. Must be greater than 1
         -> Int                 -- ^ Number of bins
         -> LogBinD
logBinDN lo rat n
  | lo  == 0  = error "Data.Histogram.Bin.LogBinD.logBinDN: zero lower bound"
  | rat <= 1  = error "Data.Histogram.Bin.LogBinD.logBinDN: increment is lesser than 1"
  | n   < 0   = error "Data.Histogram.Bin.LogBinD.logBinDN: negative number of bins"
  | otherwise = LogBinD lo rat n
  
  
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

instance SliceableBin LogBinD where
  unsafeSliceBin i j (LogBinD from step _) = LogBinD (from * step ^ i) step (j-i+1)

instance MergeableBin LogBinD where
  unsafeMergeBins dir k b@(LogBinD from step _) =
    case dir of
      CutLower  -> LogBinD (from * step^^r) (step^^k) n
      CutHigher -> LogBinD  from            (step^^k) n
    where
      n = nBins b `div` k
      r = nBins b - n * k

instance VariableBin LogBinD where
  binSizeN (LogBinD base step _) n = let x = base * step ^ n in x*step - x

instance BinEq LogBinD where
  binEq (LogBinD lo d n) (LogBinD lo' d' n')
    =  n == n'
    && abs (lo - lo') < eps * abs lo
    && abs (d  - d' ) < eps * abs d
    where
      eps = 3e-11

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

instance NFData LogBinD where
  rnf b = b `seq` ()
