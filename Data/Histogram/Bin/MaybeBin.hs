{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Data.Histogram.Bin.MaybeBin (
    MaybeBin(..)
  ) where

import Control.Monad   (liftM)
import Text.Read       (Read(..))

import Data.Histogram.Bin.Classes
import Data.Histogram.Parse


-- | This binning algorithms adds special case of no value.
newtype MaybeBin bin = MaybeBin bin
                       deriving (BinEq)

instance Bin bin => Bin (MaybeBin bin) where
  type BinValue (MaybeBin bin) = Maybe (BinValue bin)
  toIndex _            Nothing  = 0
  toIndex (MaybeBin b) (Just x) = 1 + toIndex b x

  fromIndex _            0 = Nothing
  fromIndex (MaybeBin b) i = Just (fromIndex b (i-1))

  nBins (MaybeBin b) = 1 + nBins b

instance VariableBin bin => VariableBin (MaybeBin bin) where
  binSizeN  _           0 = Nothing
  binSizeN (MaybeBin b) n = Just $ binSizeN b (n-1)

instance Show bin => Show (MaybeBin bin) where
  show (MaybeBin bin) = "# MaybeBin\n" ++ show bin

instance Read bin => Read (MaybeBin bin) where
  readPrec = do
    keyword "MaybeBin"
    liftM MaybeBin readPrec
