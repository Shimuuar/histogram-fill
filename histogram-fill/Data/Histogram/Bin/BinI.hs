{-# LANGUAGE DataKinds #-}
{-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE TypeFamilies       #-}
module Data.Histogram.Bin.BinI (
    BinI(..)
  , binI
  , binI0
  ) where

import Control.DeepSeq (NFData(..))
import Control.Monad   (liftM2)
import Data.Data       (Data,Typeable)
import Text.Read       (Read(..))

import Data.Histogram.Bin.Classes
import Data.Histogram.Bin.Read



-- | Very simple binning algorithm. Each indices. Each number
--   correcsponds to different bin.
--
--   For rountripping use 'lowerLimit' and 'upperLimit'
--
-- > b = binI (lowerLimit b) (upperLimit b)
data BinI = BinI
            !Int -- Lower bound (inclusive)
            !Int -- Upper bound (inclusive)
            deriving (Eq,Data,Typeable)

-- | Safe constructor for BinI. It checks that upper bound is
--   greater or equal than lower bound
binI :: Int                     -- ^ Lower bound (inclusive)
     -> Int                     -- ^ Upper bound (inclusive)
     -> BinI
binI lo hi | lo <= hi  = BinI lo hi
           | otherwise = error "Data.Histogram.Bin.BinI.binI: invalid paramters"

-- | Construct BinI with n bins. Indexing starts from 0. n must be positive
binI0 :: Int                    -- ^ Number of bins.
      -> BinI
binI0 n = binI 0 (n - 1)

instance Bin BinI where
  type BinValue    BinI = Int
  type BinIdx      BinI = IndexUO
  type BinValueSet BinI = Range1D 'DomEnumeration Int
  toIndex (BinI a b) x
    | x < a     = IdxU
    | x > b     = IdxO
    | otherwise = IdxN (x - a)
  fromIndex (BinI a b) = \case
    IdxU   -> LessThan a
    IdxN i -> Point   (a + i)
    IdxO   -> GEqThan (b+1)
  isIndexValid (BinI a b) = \case
    IdxU   -> True
    IdxN i -> i >= 0 && i <= (b - a)
    IdxO   -> True
  --
  toBufferIdx (BinI a b) = \case
    IdxU   -> 0
    IdxN i -> i + 1
    IdxO   -> b - a + 2
  fromBufferIdx (BinI a b) i
    | i == 0         = IdxU
    | i == b - a + 2 = IdxO
    | otherwise      = IdxN (i - 1)
  totalBinNumber (BinI a b) = b - a + 3

instance Bin1D BinI where
  lowerLimit (BinI i _) = i
  upperLimit (BinI _ i) = i

instance SliceableBin BinI where
  sliceBin i j = undefined

instance UniformBin BinI where
  binSize _ = 1

instance Show BinI where
  show (BinI lo hi) = unlines [ "# BinI"
                              , "# Low  = " ++ show lo
                              , "# High = " ++ show hi
                              ]
instance Read BinI where
  readPrec = keyword "BinI" >> liftM2 BinI (value "Low") (value "High")

instance NFData BinI where
  rnf b = b `seq` ()
