{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Data.Histogram.Bin.BinEnum (
    BinEnum(..)
  , binEnum
  , binEnumFull
  ) where

import Control.DeepSeq (NFData(..))
import Control.Monad   (liftM)
import Data.Data       (Data,Typeable)
import Text.Read       (Read(..))

import Data.Histogram.Bin.Classes
import Data.Histogram.Bin.BinI
import Data.Histogram.Bin.Read

-- | Bin for types which are instnaces of Enum type class. Value are
--   converted to 'Int' using 'fromEnum' first and then binned.
newtype BinEnum a = BinEnum BinI
                    deriving (Eq,Data,Typeable,BinEq)

-- | Create enum based bin
binEnum :: Enum a => a -> a -> BinEnum a
binEnum a b = BinEnum $ binI (fromEnum a) (fromEnum b)

-- | Use full range of data
binEnumFull :: (Enum a, Bounded a) => BinEnum a
binEnumFull = binEnum minBound maxBound

instance Enum a => Bin (BinEnum a) where
  type BinValue (BinEnum a) = a
  toIndex   (BinEnum b) = toIndex b . fromEnum
  fromIndex (BinEnum b) = toEnum . fromIndex b
  inRange   (BinEnum b) = inRange b . fromEnum
  nBins     (BinEnum b) = nBins b

instance (Enum a, Ord a) => IntervalBin (BinEnum a) where
  binInterval b x = (n,n) where n = fromIndex b x

instance (Enum a, Ord a) => Bin1D (BinEnum a) where
  lowerLimit (BinEnum b) = toEnum $ lowerLimit b
  upperLimit (BinEnum b) = toEnum $ upperLimit b

instance (Enum a, Ord a) => SliceableBin (BinEnum a) where
  unsafeSliceBin i j (BinEnum b) = BinEnum $ unsafeSliceBin i j b

instance Show (BinEnum a) where
  show (BinEnum b) = "# BinEnum\n" ++ show b
instance Read (BinEnum a) where
  readPrec = keyword "BinEnum" >> liftM BinEnum readPrec

instance NFData (BinEnum a) where
  rnf b = b `seq` ()
