-- Yes I DO want orphans here
{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE FlexibleInstances #-}
module QC.Instances() where

import Control.Applicative
import Test.QuickCheck
import qualified Data.Vector.Unboxed as U

import Data.Histogram
import Data.Histogram.Bin.MaybeBin



----------------------------------------------------------------
-- Bin instances
----------------------------------------------------------------

instance Arbitrary BinI where
  arbitrary = do
    let maxI = 100
    lo <- choose (-maxI , maxI)
    hi <- choose (lo    , maxI)
    return $ binI lo hi

instance Arbitrary BinInt where
  arbitrary = do
    let maxI = 100
    base <- choose (-maxI,maxI)
    step <- choose (1,10)
    n    <- choose (1,1000)
    return $ BinInt base step n

instance (Arbitrary a, Ord a, Enum a) => Arbitrary (BinEnum a) where
  arbitrary = do
    l <- arbitrary
    h <- suchThat arbitrary (>= l)
    return $ binEnum l h

instance Arbitrary (BinF Float) where
  arbitrary = do
    lo <- choose (-1.0e+3-1 , 1.0e+3)
    n  <- choose (1, 1000)
    hi <- choose (lo , 1.0e+3+1)
    return $ binF lo n hi

instance Arbitrary (BinF Double) where
  arbitrary = do
    lo <- choose (-1.0e+6-1 , 1.0e+6)
    n  <- choose (1, 1000*1000)
    hi <- choose (lo , 1.0e+6+1)
    return $ binF lo n hi

instance Arbitrary BinD where
  arbitrary = do
    lo <- choose (-1.0e+6-1 , 1.0e+6)
    n  <- choose (1, 1000*1000)
    hi <- choose (lo , 1.0e+6+1)
    return $ binD lo n hi

instance Arbitrary LogBinD where
  arbitrary = do
    lo <- choose (1.0e-6 , 1.0e+6)
    n  <- choose (1, 1000*1000)
    hi <- choose (lo , 1.0e+6+1)
    return $ logBinD lo n hi

instance Arbitrary bin => Arbitrary (MaybeBin bin) where
  arbitrary = MaybeBin <$> arbitrary

instance (Arbitrary bx, Arbitrary by) => Arbitrary (Bin2D bx by) where
    arbitrary = Bin2D <$> arbitrary <*> arbitrary

----------------------------------------------------------------
-- Histogram instance
----------------------------------------------------------------

instance (Bin bin, U.Unbox a, Arbitrary bin, Arbitrary a) => Arbitrary (Histogram bin a) where
    arbitrary = do
      bin <- suchThat arbitrary ((<333) . nBins)
      histogramUO bin <$> arbitrary <*> (U.fromList <$> vectorOf (nBins bin) arbitrary)
