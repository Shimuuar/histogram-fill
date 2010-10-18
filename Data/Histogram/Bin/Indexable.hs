{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.Histogram.Bin.Indexable (
    -- * 2D indexable bins
    Indexable2D(..)
  , BinIx2D
  , binIx2D
  ) where

import Text.Read            (Read(..))

import Data.Histogram.Bin
import Data.Histogram.Parse



-- | This type class is same as Indexable but for 2D values.
class Indexable2D a where
  -- | Convert value to index
  index2D :: a -> (Int,Int)
  -- | Convert index to value
  deindex2D :: (Int,Int) -> a

instance (Enum a, Enum b) => Indexable2D (a,b) where
  index2D   (x,y) = (fromEnum x, fromEnum y)
  deindex2D (i,j) = (toEnum   i, toEnum   j)

----------------------------------------------------------------
-- Indexed 2D bins
----------------------------------------------------------------
-- | Binning for 2D indexable value
newtype BinIx2D i = BinIx2D (Bin2D BinI BinI)

-- | Construct indexed bin
binIx2D :: Indexable2D i => i -> i -> BinIx2D i
binIx2D lo hi = let (ix,iy) = index2D lo
                    (jx,jy) = index2D hi
                in BinIx2D $ BinI ix jx >< BinI iy jy

instance Indexable2D i => Bin (BinIx2D i) where
    type BinValue (BinIx2D i) = i
    toIndex   (BinIx2D b) x = toIndex b (index2D x)
    fromIndex (BinIx2D b) i = deindex2D $ fromIndex b i
    inRange   (BinIx2D b) x = inRange b (index2D x)
    nBins     (BinIx2D b)   = nBins b

instance (Show i, Indexable2D i) => Show (BinIx2D i) where
    show (BinIx2D b) = unlines [ "# BinIx2D"
                               , "# Low  = " ++ show (deindex2D (fromIndex b 0            ) :: i)
                               , "# High = " ++ show (deindex2D (fromIndex b (nBins b - 1)) :: i)
                               ]
instance (Read i, Indexable2D i) => Read (BinIx2D i) where
    readPrec = do
      keyword "BinIx2D"
      l <- value "Low"
      h <- value "High"
      return $ binIx2D l h
