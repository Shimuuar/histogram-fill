{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- Requred for Bin2D conversions
{-# LANGUAGE OverlappingInstances #-}
-- |
-- Module     : Data.Histogram.Bin
-- Copyright  : Copyright (c) 2009, Alexey Khudyakov <alexey.skladnoy@gmail.com>
-- License    : BSD3
-- Maintainer : Alexey Khudyakov <alexey.skladnoy@gmail.com>
-- Stability  : experimental
--
-- Binning algorithms. This is mapping from set of interest to integer
-- indices and approximate reverse.

module Data.Histogram.Bin ( -- * Type classes
                            Bin(..)
                          , binsCenters
                            -- ** 1D bins
                          , IntervalBin(..)
                          , Bin1D(..)
                          , GrowBin(..)
                            -- ** Bin sizes
                          , UniformBin(..)
                          , VariableBin(..)
                            -- ** Conversion
                          , ConvertBin(..)
                          -- * Bin types
                          -- ** Integer bins
                          , BinI(..)
                          , binI0
                          -- ** Integer bins with non-1 size
                          , BinInt(..)
                          , binInt
                          -- ** Enum based bin
                          , BinEnum(..)
                          , binEnum
                          , binEnumFull
                          -- ** Floating point bins
                          , BinF
                          , binF
                          , binFn
                          , binFstep
                          , scaleBinF
                          -- *** Specialized for Double
                          , BinD
                          , binD
                          , binDn
                          , binDstep
                          , scaleBinD
                          -- ** Log scale point
                          , LogBinD
                          , logBinD
                          -- ** 2D bins
                          , Bin2D(..)
                          , (><)
                          , nBins2D
                          , toIndex2D
                          , fmapBinX
                          , fmapBinY
                          ) where

import Control.Monad (liftM, liftM2, liftM3)
import GHC.Float     (double2Int)

import qualified Data.Vector.Generic as G
import           Data.Vector.Generic    (Vector)
import Data.Typeable                    (Typeable)
import Data.Data                        (Data)
import Text.Read                        (Read(..))

import Data.Histogram.Parse



----------------------------------------------------------------
-- Type classes
----------------------------------------------------------------

-- | This type represent some abstract data binning algorithms. Such
--   algorithm maps sets of values to integer indices.
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
  -- implementation. Should satisfy:
  -- inRange b x &#8660; toIndex b x &#8712; [0,nBins b)
  inRange :: b -> BinValue b -> Bool
  inRange b x = i >= 0 && i < nBins b where i = toIndex b x

---- 1D bins ---------------------------------------------------

-- | For binning algorithms which work with bin values which have some
--   natural ordering and every bin is continous interval.
class Bin b => IntervalBin b where
  -- | Interval for n'th bin
  binInterval :: b -> Int -> (BinValue b, BinValue b)
  -- | List of all bins. Could be overridden for efficiency.
  binsList :: Vector v (BinValue b, BinValue b) => b -> v (BinValue b, BinValue b)
  binsList b = G.generate (nBins b) (binInterval b)
  {-# INLINE binsList #-}


-- | IntervalBin for which domain is single inerval
class IntervalBin b => Bin1D b where
  -- | Minimal accepted value of histogram
  lowerLimit :: b -> BinValue b
  -- | Maximal accepted value of histogram
  upperLimit :: b -> BinValue b
  -- | Slice bin by indices
  unsafeSliceBin :: Int -> Int -> b -> b

  
-- | Binning algorithm which individual 
class Bin1D b => GrowBin b where
  -- | Set numbers to zero. By convention bins are shrinked to lower bound
  zeroBin    :: b -> b
  -- | Append one bin at upper bound
  appendBin  :: b -> b
  -- | Prepend one bin at lower bin
  prependBin :: b -> b


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

binsCenters :: (Bin b, Vector v (BinValue b)) => b -> v (BinValue b)
binsCenters b = G.generate (nBins b) (fromIndex b)
{-# INLINE binsCenters #-}

----------------------------------------------------------------
-- Integer bin
----------------------------------------------------------------
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

-- | Construct BinI with n bins. Indexing starts from 0
binI0 :: Int -> BinI
binI0 n = BinI 0 (n-1)

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
  unsafeSliceBin i j (BinI l _) = BinI (l+i) (j-i+1)

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



----------------------------------------------------------------
-- Another form of Integer bin
----------------------------------------------------------------

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

-- | Construct BinInt.
binInt :: Int                   -- ^ Lower bound
       -> Int                   -- ^ Bin size
       -> Int                   -- ^ Upper bound
       -> BinInt
binInt lo n hi = BinInt lo n nb
  where
    nb = (hi-lo) `div` n

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


----------------------------------------------------------------
-- Enumeration bin
----------------------------------------------------------------

-- | Bin for types which are instnaces of Enum type class
newtype BinEnum a = BinEnum BinI
                    deriving (Eq,Data,Typeable,GrowBin)

-- | Create enum based bin
binEnum :: Enum a => a -> a -> BinEnum a
binEnum a b = BinEnum $ BinI (fromEnum a) (fromEnum b)

-- | Use full range of data
binEnumFull :: (Enum a, Bounded a) => BinEnum a
binEnumFull = binEnum minBound maxBound

instance Enum a => Bin (BinEnum a) where
  type BinValue (BinEnum a) = a
  toIndex   (BinEnum b) = toIndex b . fromEnum
  fromIndex (BinEnum b) = toEnum . fromIndex b
  inRange   (BinEnum b) = inRange b . fromEnum
  nBins     (BinEnum b) = nBins b

instance Enum a => IntervalBin (BinEnum a) where
  binInterval b x = (n,n) where n = fromIndex b x

instance Enum a => Bin1D (BinEnum a) where
  lowerLimit (BinEnum b) = toEnum $ lowerLimit b
  upperLimit (BinEnum b) = toEnum $ upperLimit b
  unsafeSliceBin i j (BinEnum b) = BinEnum $ unsafeSliceBin i j b

instance Show (BinEnum a) where
  show (BinEnum b) = "# BinEnum\n" ++ show b
instance Read (BinEnum a) where
  readPrec = keyword "BinEnum" >> liftM BinEnum readPrec



----------------------------------------------------------------
-- Floating point bin
----------------------------------------------------------------

-- | Floaintg point bins with equal sizes.
--
-- Note that due to GHC bug #2271 this toIndex is really slow (20x
-- slowdown with respect to BinD) and use of BinD is recommended
--
-- 1. Lower bound
--
-- 2. Size of bin
--
-- 3. Number of bins
data BinF f = BinF {-# UNPACK #-} !f   -- Lower bound
                   {-# UNPACK #-} !f   -- Size of bin
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

instance RealFrac f => GrowBin (BinF f) where
  zeroBin    (BinF from step _) = BinF from step 0
  appendBin  (BinF from step n) = BinF from step (n+1)
  prependBin (BinF from step n) = BinF (from-step) step (n+1)

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



----------------------------------------------------------------
-- Floating point bin /Specialized for Double
----------------------------------------------------------------
-- | Floaintg point bins with equal sizes. If you work with Doubles
-- this data type should be used instead of BinF.
--
-- 1. Lower bound
--
-- 2. Size of bin
--
-- 3. Number of bins
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

instance GrowBin BinD where
  zeroBin    (BinD from step _) = BinD from step 0
  appendBin  (BinD from step n) = BinD from step (n+1)
  prependBin (BinD from step n) = BinD (from-step) step (n+1)

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



----------------------------------------------------------------
-- Log-scale bin
----------------------------------------------------------------
-- | Logarithmic scale bins.
--
-- 1. Lower bound
--
-- 2. Upper bound
--
-- 2. Increment ratio
--
-- 3. Number of bins
data LogBinD = LogBinD
               Double -- Low border
               Double -- Hi border
               Double -- Increment ratio
               Int    -- Number of bins
               deriving (Eq,Typeable)

-- | Create log-scale bins.
logBinD :: Double -> Int -> Double -> LogBinD
logBinD lo n hi = LogBinD lo hi ((hi/lo) ** (1 / fromIntegral n)) n

instance Bin LogBinD where
  type BinValue LogBinD = Double
  toIndex   !(LogBinD base _ step _) !x = floorD $ logBase step (x / base)
  fromIndex !(LogBinD base _ step _) !i | i >= 0    = base * step ** (fromIntegral i + 0.5)
                                        | otherwise = -1 / 0
  inRange   !(LogBinD lo hi _ _) x  = x >= lo && x < hi
  nBins     !(LogBinD _ _ _ n) = n
  {-# INLINE toIndex #-}

instance IntervalBin LogBinD where
  binInterval (LogBinD base _ step _) i = (x, x*step) where x = base * step ** (fromIntegral i)

instance Bin1D LogBinD where
  lowerLimit (LogBinD lo _  _ _) = lo
  upperLimit (LogBinD _  hi _ _) = hi
  unsafeSliceBin i j (LogBinD from q step _) = LogBinD (from * step ^ i) q step (j-i+1)

instance VariableBin LogBinD where
  binSizeN (LogBinD base _ step _) n = let x = base * step ^ n in x*step - x

instance Show LogBinD where
  show (LogBinD lo hi _ n) =
    unlines [ "# LogBinD"
            , "# Lo   = " ++ show lo
            , "# N    = " ++ show n
            , "# Hi   = " ++ show hi
            ]
instance Read LogBinD where
  readPrec = do
    keyword "LogBinD"
    liftM3 logBinD (value "Lo") (value "N") (value "Hi")


----------------------------------------------------------------
-- 2D bin
----------------------------------------------------------------

-- | 2D bins. binX is binning along X axis and binY is one along Y axis.
data Bin2D binX binY = Bin2D { binX :: !binX -- ^ Binning algorithm for X axis
                             , binY :: !binY -- ^ Binning algorithm for Y axis
                             }
                       deriving (Eq,Data,Typeable)

-- | Alias for 'Bin2D'.
(><) :: binX -> binY -> Bin2D binX binY
(><) = Bin2D

instance (Bin binX, Bin binY) => Bin (Bin2D binX binY) where
  type BinValue (Bin2D binX binY) = (BinValue binX, BinValue binY)
  toIndex !(Bin2D bx by) !(x,y)
        | inRange bx x = toIndex bx x + toIndex by y * nBins bx
        | otherwise    = maxBound
  fromIndex b@(Bin2D bx by) i = let (ix,iy) = toIndex2D b i
                                in  (fromIndex bx ix, fromIndex by iy)
  inRange (Bin2D bx by) !(x,y) = inRange bx x && inRange by y
  nBins (Bin2D bx by) = nBins bx * nBins by
  {-# INLINE toIndex #-}

-- | Convert index into pair of indices for X and Y axes
toIndex2D :: (Bin binX, Bin binY) => Bin2D binX binY -> Int -> (Int,Int)
toIndex2D !b !i = let (iy,ix) = divMod i (nBins $ binX b) in (ix,iy)
{-# INLINE toIndex2D #-}

-- | 2-dimensional size of binning algorithm
nBins2D :: (Bin bx, Bin by) => Bin2D bx by -> (Int,Int)
nBins2D (Bin2D bx by) = (nBins bx, nBins by)

-- | Apply function to X binning algorithm. If new binning algorithm
--   have different number of bins will fail.
fmapBinX :: (Bin bx, Bin bx') => (bx -> bx') -> Bin2D bx by -> Bin2D bx' by
fmapBinX f (Bin2D bx by)
    | nBins bx' /= nBins bx = error "fmapBinX: new binnig algorithm has different number of bins"
    | otherwise             = Bin2D bx' by
    where
      bx' = f bx

-- | Apply function to Y binning algorithm. If new binning algorithm
--   have different number of bins will fail.
fmapBinY ::(Bin by, Bin by') => (by -> by') -> Bin2D bx by -> Bin2D bx by'
fmapBinY f (Bin2D bx by)
    | nBins by' /= nBins by = error "fmapBinY: new binnig algorithm has different number of bins"
    | otherwise             = Bin2D bx by'
    where
      by' = f by

instance (Show b1, Show b2) => Show (Bin2D b1 b2) where
  show (Bin2D b1 b2) = concat [ "# Bin2D\n"
                              , "# X\n"
                              , show b1
                              , "# Y\n"
                              , show b2
                              ]
instance (Read b1, Read b2) => Read (Bin2D b1 b2) where
  readPrec = do
    keyword "Bin2D"
    keyword "X"
    b1 <- readPrec
    keyword "Y"
    b2 <- readPrec
    return $ Bin2D b1 b2

----------------------------------------------------------------
-- Bin conversion
----------------------------------------------------------------

-- BinI,BinInt -> BinF
instance RealFrac f => ConvertBin BinI (BinF f) where
  convertBin b = BinF (fromIntegral (lowerLimit b) - 0.5) 1 (nBins b)
instance RealFrac f => ConvertBin BinInt (BinF f) where
  convertBin b = BinF (fromIntegral (lowerLimit b) - 0.5) (fromIntegral $ binSize b) (nBins b)

-- BinI,BinInt -> BinD
instance ConvertBin BinI BinD where
  convertBin b = BinD (fromIntegral (lowerLimit b) - 0.5) 1 (nBins b)
instance ConvertBin BinInt BinD where
  convertBin b = BinD (fromIntegral (lowerLimit b) - 0.5) (fromIntegral $ binSize b) (nBins b)

-- Bin2D -> Bin2D
instance (ConvertBin bx bx', Bin by) => ConvertBin (Bin2D bx by) (Bin2D bx' by) where
  convertBin = fmapBinX convertBin
instance (ConvertBin by by', Bin bx) => ConvertBin (Bin2D bx by) (Bin2D bx by') where
  convertBin = fmapBinY convertBin
instance (ConvertBin bx bx', ConvertBin by by') => ConvertBin (Bin2D bx by) (Bin2D bx' by') where
  convertBin (Bin2D bx by) = Bin2D (convertBin bx) (convertBin by)
