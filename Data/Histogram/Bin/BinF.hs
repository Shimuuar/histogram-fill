{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Data.Histogram.Bin.BinF (
    -- * Generic and slow
    BinF
  , binF
  , binFn
  , binFstep
  , scaleBinF
    -- * Specialized for Double and fast
  , BinD
  , binD
  , binDn
  , binDstep
  , scaleBinD
  ) where

import Control.DeepSeq (NFData(..))
import Control.Monad   (liftM3)
import GHC.Float       (double2Int)
import Data.Data       (Data,Typeable)
import Text.Read       (Read(..))

import Data.Histogram.Bin.Classes
import Data.Histogram.Parse


-- | Floaintg point bins with equal sizes.
--
--   Since 'BinF' is paramentric it couldn't be unpacked. So @BinF
--   Double@ will be always slower than 'BinD'. For roundtripping use:
--
-- > b = binFstep (lowerLimit b) (binSize b) (nBins b)
data BinF f = BinF !f                  -- Lower bound
                   !f                  -- Size of bin
                   {-# UNPACK #-} !Int -- Number of bins
              deriving (Eq,Data,Typeable)

-- | Create bins.
binF :: RealFrac f =>
        f   -- ^ Lower bound of range
     -> Int -- ^ Number of bins
     -> f   -- ^ Upper bound of range
     -> BinF f
binF from n to = BinF from ((to - from) / fromIntegral n) n

-- | Create bins. Note that actual upper bound can differ from specified.
binFn :: RealFrac f =>
         f -- ^ Begin of range
      -> f -- ^ Size of step
      -> f -- ^ Approximation of end of range
      -> BinF f
binFn from step to = BinF from step (round $ (to - from) / step)

-- | Create bins
binFstep :: RealFrac f =>
            f      -- ^ Begin of range
         -> f      -- ^ Size of step
         -> Int    -- ^ Number of bins
         -> BinF f
binFstep = BinF

-- | 'scaleBinF a b' scales BinF using linear transform 'a+b*x'
scaleBinF :: RealFrac f => f -> f -> BinF f -> BinF f
scaleBinF a b (BinF base step n)
    | b > 0     = BinF (a + b*base) (b*step) n
    | otherwise = error $ "scaleBinF: b must be positive (b = "++show b++")"

instance RealFrac f => Bin (BinF f) where
  type BinValue (BinF f) = f
  toIndex   !(BinF from step _) !x = floor $ (x-from) / step
  fromIndex !(BinF from step _) !i = (step/2) + (fromIntegral i * step) + from
  nBins     !(BinF _ _ n) = n
  {-# INLINE toIndex #-}

instance RealFrac f => IntervalBin (BinF f) where
  binInterval (BinF from step _) i = (x, x + step) where x = from + step * fromIntegral i

instance RealFrac f => Bin1D (BinF f) where
  lowerLimit (BinF from _    _) = from
  upperLimit (BinF from step n) = from + step * fromIntegral n
  unsafeSliceBin i j (BinF from step _) = BinF (from + step * fromIntegral i) step (j-i+1)

instance RealFrac f => VariableBin (BinF f) where
  binSizeN (BinF _ step _) _ = step

instance RealFrac f => UniformBin (BinF f) where
  binSize (BinF _ step _) = step

instance Show f => Show (BinF f) where
  show (BinF base step n) = unlines [ "# BinF"
                                    , "# Base = " ++ show base
                                    , "# Step = " ++ show step
                                    , "# N    = " ++ show n
                                    ]
instance (Read f, RealFrac f) => Read (BinF f) where
  readPrec = keyword "BinF" >> liftM3 BinF (value "Base") (value "Step") (value "N")

instance NFData (BinF f)



----------------------------------------------------------------
-- Floating point bin /Specialized for Double
----------------------------------------------------------------
-- | Floaintg point bins with equal sizes. If you work with Doubles
-- this data type should be used instead of 'BinF'. Roundtripping is same as with 'BinF'
data BinD = BinD {-# UNPACK #-} !Double -- Lower bound
                 {-# UNPACK #-} !Double -- Size of bin
                 {-# UNPACK #-} !Int    -- Number of bins
            deriving (Eq,Data,Typeable)

-- | Create bins.
binD :: Double -- ^ Lower bound of range
     -> Int    -- ^ Number of bins
     -> Double -- ^ Upper bound of range
     -> BinD
binD from n to = BinD from ((to - from) / fromIntegral n) n

-- | Create bins. Note that actual upper bound can differ from specified.
binDn :: Double -- ^ Begin of range
      -> Double -- ^ Size of step
      -> Double -- ^ Approximation of end of range
      -> BinD
binDn from step to = BinD from step (round $ (to - from) / step)

-- | Create bins
binDstep :: Double -- ^ Begin of range
         -> Double -- ^ Size of step
         -> Int    -- ^ Number of bins
         -> BinD
binDstep = BinD

-- | 'scaleBinF a b' scales BinF using linear transform 'a+b*x'
scaleBinD :: Double -> Double -> BinD -> BinD
scaleBinD a b (BinD base step n)
    | b > 0     = BinD (a + b*base) (b*step) n
    | otherwise = error $ "scaleBinF: b must be positive (b = "++show b++")"

-- Fast variant of flooor
floorD :: Double -> Int
floorD x | x < 0     = double2Int x - 1
         | otherwise = double2Int x
{-# INLINE floorD #-}

instance Bin BinD where
  type BinValue BinD = Double
  toIndex   !(BinD from step _) !x = floorD $ (x-from) / step
  fromIndex !(BinD from step _) !i = (step/2) + (fromIntegral i * step) + from
  nBins     !(BinD _ _ n) = n
  {-# INLINE toIndex #-}

instance IntervalBin BinD where
  binInterval (BinD from step _) i = (x, x + step) where x = from + step * fromIntegral i

instance Bin1D BinD where
  lowerLimit (BinD from _    _) = from
  upperLimit (BinD from step n) = from + step * fromIntegral n
  unsafeSliceBin i j (BinD from step _) = BinD (from + step * fromIntegral i) step (j-i+1)

instance VariableBin BinD where
  binSizeN (BinD _ step _) _ = step

instance UniformBin BinD where
  binSize (BinD _ step _) = step

instance Show BinD where
  show (BinD base step n) = unlines [ "# BinD"
                                    , "# Base = " ++ show base
                                    , "# Step = " ++ show step
                                    , "# N    = " ++ show n
                                    ]
instance Read BinD where
  readPrec = keyword "BinD" >> liftM3 BinD (value "Base") (value "Step") (value "N")

instance NFData BinD
