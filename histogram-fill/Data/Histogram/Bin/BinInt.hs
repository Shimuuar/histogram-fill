{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Data.Histogram.Bin.BinInt (
    BinInt(..)
  , binInt
  , binIntN
  , binIntStep
  ) where

import Control.DeepSeq (NFData(..))
import Control.Monad   (liftM3)
import Data.Data       (Data,Typeable)
import Text.Read       (Read(..))

import Data.Histogram.Bin.Classes
import Data.Histogram.Bin.Read



-- | Integer bins of equal size. For roundtripping use:
--
-- > b = binIntStep (lowerLimit b) (binSize b) (nBins b)
data BinInt = BinInt
              {-# UNPACK #-} !Int -- Low bound
              {-# UNPACK #-} !Int -- Bin size
              {-# UNPACK #-} !Int -- Number of bins
              deriving (Eq,Data,Typeable)


-- | Construct BinInt.
binInt :: Int                   -- ^ Lower bound
       -> Int                   -- ^ Bin size
       -> Int                   -- ^ Upper bound
       -> BinInt
binInt lo n hi 
  | n  < 0    = error "Data.Histogram.Bin.BinInt.binInt: negative bin size"
  | hi < lo   = binInt hi n lo
  | otherwise = BinInt lo n nb
  where
    nb = (hi-lo) `div` n

-- | Construct 'BinInt'.
binIntN :: Int                  -- ^ Lower bound
        -> Int                  -- ^ Bin size
        -> Int                  -- ^ Upper bound
        -> BinInt
binIntN lo n hi
  | n  < 0    = error "Data.Histogram.Bin.BinInt.binIntN: negative bin size"
  | hi < lo   = binIntN hi n lo
  | n  >= rng = BinInt lo 1 rng
  | otherwise = BinInt lo (rng `div` n) size
  where
    size = rng `div` n
    rng  = hi - lo + 1

binIntStep :: Int               -- ^ Lower bound
           -> Int               -- ^ Bin size
           -> Int               -- ^ Number of bins
           -> BinInt
binIntStep lo step n
  | step < 0  = error "Data.Histogram.Bin.BinInt.binIntStep: negative number of bins"
  | n    < 0  = error "Data.Histogram.Bin.BinInt.binIntStep: negative bin size"
  | otherwise = BinInt lo step n

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

instance SliceableBin BinInt where
  unsafeSliceBin i j (BinInt base sz _) = BinInt (base + i*sz) sz (j-i+1)

instance MergeableBin BinInt where
  unsafeMergeBins dir k b@(BinInt base step _) =
    case dir of
      CutLower  -> BinInt (base + r) (step*k) n
      CutHigher -> BinInt  base      (step*k) n
    where
      n = nBins b `div` k
      r = (nBins b - n * k) * step

instance VariableBin BinInt where
  binSizeN (BinInt _ sz _) _ = sz

instance UniformBin BinInt where
  binSize (BinInt _ sz _) = sz

instance BinEq BinInt where
  binEq = (==)

instance Show BinInt where
  show (BinInt base sz n) =
    unlines [ "# BinInt"
            , "# Base = " ++ show base
            , "# Step = " ++ show sz
            , "# Bins = " ++ show n
            ]

instance Read BinInt where
  readPrec = keyword "BinInt" >> liftM3 BinInt (value "Base") (value "Step") (value "Bins")

instance NFData BinInt where
  rnf b = b `seq` ()
