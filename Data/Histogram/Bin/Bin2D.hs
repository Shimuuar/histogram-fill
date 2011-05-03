{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Data.Histogram.Bin.Bin2D (
    Bin2D(..)
  , (><)
  , nBins2D
  , fmapBinX
  , fmapBinY
  ) where

import Data.Typeable (Typeable)
import Data.Data     (Data)
import Text.Read     (Read(..))

import Data.Histogram.Bin.Classes
import Data.Histogram.Parse

-- | 2D bins. binX is binning along X axis and binY is one along Y
--   axis. Data is stored in row-major order
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