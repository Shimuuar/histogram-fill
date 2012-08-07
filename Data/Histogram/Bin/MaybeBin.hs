{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Data.Histogram.Bin.MaybeBin (
    MaybeBin(..)
  , fromMaybeBin
  ) where

import Control.Monad   (liftM)
import Data.Typeable   (Typeable)
import Text.Read       (Read(..))

import Data.Histogram.Bin.Classes
import Data.Histogram.Bin.Read
import qualified Data.Vector.Generic    as G
import qualified Data.Histogram.Generic as H



-- | This binning algorithms adds special case of no value.
newtype MaybeBin bin = MaybeBin bin
                       deriving (BinEq,Eq,Typeable)

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


-- | Drop bin with no events
fromMaybeBin :: (Bin b, G.Vector v a) => H.Histogram v (MaybeBin b) a -> H.Histogram v b a
fromMaybeBin h = H.histogram b (G.tail $ H.histData h)
  where
    MaybeBin b = H.bins h
