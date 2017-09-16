{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | Arbitrary instances for histogram-fill
module Data.Histogram.QuickCheck where

import Control.Applicative
import Test.QuickCheck
import Data.List
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Generic as G
import System.Random (Random)

-- import Data.Histogram
-- import Data.Histogram.Bin.MaybeBin
-- import Data.Histogram.Bin.BinVar

import Data.Histogram.Bin.Classes
import Data.Histogram.Bin.BinI
import Data.Typeable

----------------------------------------------------------------
-- Properties
----------------------------------------------------------------

-- p_BufIdxRoundtrip :: (Bin b, Eq (BinIdx b)) => Proxy b -> b -> BinIdx b -> Bool
-- p_BufIdxRoundtrip _ bin i
--   =  isIndexValid bin i
--   && (i == fromBufferIdx bin (toBufferIdx bin i))
p_roundtripIdx2Buf
  :: (Bin bin, Show (BinIdx bin), Eq (BinIdx bin))
  => p bin -> WithIdx bin -> Property
p_roundtripIdx2Buf _ (WithIdx bin idx)
  = counterexample ("buf idx = " ++ show i)
  $ counterexample ("index'  = " ++ show idx')
  $ and [ isIndexValid bin idx
        , i >= 0
        , i <  totalBinNumber bin
        , idx == idx'
        ]
  where
    i    = toBufferIdx   bin idx
    idx' = fromBufferIdx bin i

p_roundtripBuf2Idx
  :: (Bin bin, Show (BinIdx bin))
  => p bin -> WithBufIdx bin -> Property
p_roundtripBuf2Idx _ (WithBufIdx bin i)
  = counterexample ("index        = " ++ show idx)
  $ counterexample ("new buf. idx = " ++ show i')
  $ and [ isIndexValid bin idx
        , i >= 0
        , i <  totalBinNumber bin
        , i == i'
        ]
  where
    idx = fromBufferIdx bin i
    i'  = toBufferIdx   bin idx


----------------------------------------------------------------
-- Bin instances
----------------------------------------------------------------

go p = do
  quickCheck (p_roundtripBuf2Idx p)
  quickCheck (p_roundtripIdx2Buf p)


-- |
data WithBufIdx bin = WithBufIdx bin Int

instance Show bin => Show (WithBufIdx bin) where
  show (WithBufIdx bin i) = show bin ++ "\nbuffer index = " ++ show i

instance (Arbitrary bin, Bin bin) => Arbitrary (WithBufIdx bin) where
  arbitrary = do
    bin <- arbitrary
    i   <- choose (0, totalBinNumber bin - 1)
    return (WithBufIdx bin i)


-- |
data WithIdx bin = WithIdx bin (BinIdx bin)

instance (Show bin, Show (BinIdx bin))  => Show (WithIdx bin) where
  show (WithIdx bin i) = show bin ++ "\nindex = " ++ show i

instance ArbitraryBin bin => Arbitrary (WithIdx bin) where
  arbitrary = do
    bin <- arbitrary
    i   <- arbitraryIdx bin
    return (WithIdx bin i)

----------------------------------------------------------------

class Arbitrary bin => ArbitraryBin bin where
  arbitraryIdx :: bin -> Gen (BinIdx bin)

instance Arbitrary BinI where
  arbitrary = do
    let maxI = 100
    lo <- choose (-maxI , maxI)
    hi <- choose (lo    , maxI)
    return $ binI lo hi

instance ArbitraryBin BinI where
  arbitraryIdx (BinI a b) = do
    let n = b - a + 1
    frequency [ (1,  pure IdxU)
              , (10, IdxN <$> choose (0, b - a))
              , (1,  pure IdxO)
              ]

-- instance Arbitrary BinInt where
--   arbitrary = do
--     let maxI = 100
--     base <- choose (-maxI,maxI)
--     step <- choose (1,10)
--     n    <- choose (1,1000)
--     return $ BinInt base step n

-- instance (Arbitrary a, Ord a, Enum a) => Arbitrary (BinEnum a) where
--   arbitrary = do
--     l <- arbitrary
--     h <- suchThat arbitrary (>= l)
--     return $ binEnum l h

-- instance Arbitrary (BinF Float) where
--   arbitrary = do
--     lo <- choose (-1.0e+3-1 , 1.0e+3)
--     n  <- choose (1, 1000)
--     hi <- choose (lo , 1.0e+3+1)
--     return $ binF lo n hi

-- instance Arbitrary (BinF Double) where
--   arbitrary = do
--     lo <- choose (-1.0e+6-1 , 1.0e+6)
--     n  <- choose (1, 1000*1000)
--     hi <- choose (lo , 1.0e+6+1)
--     return $ binF lo n hi

-- instance Arbitrary BinD where
--   arbitrary = do
--     lo <- choose (-1.0e+6-1 , 1.0e+6)
--     n  <- choose (1, 1000*1000)
--     hi <- choose (lo , 1.0e+6+1)
--     return $ binD lo n hi

-- instance Arbitrary LogBinD where
--   arbitrary = do
--     lo <- choose (1.0e-6 , 1.0e+6)
--     n  <- choose (1, 1000*1000)
--     hi <- choose (lo , 1.0e+6+1)
--     return $ logBinD lo n hi

-- instance Arbitrary bin => Arbitrary (MaybeBin bin) where
--   arbitrary = MaybeBin <$> arbitrary

-- instance (Arbitrary bx, Arbitrary by) => Arbitrary (Bin2D bx by) where
--   arbitrary = Bin2D <$> arbitrary <*> arbitrary

-- instance (Arbitrary a, Ord a, G.Vector v a, G.Vector v Bool
--          ) => Arbitrary (BinVarG v a) where
--   arbitrary = do
--     n     <- choose (2,333)
--     cuts' <- vector n `suchThat` (\x -> nub x == x)
--     return $ binVar $ G.fromList $ sort cuts'
--   shrink = fmap (binVar . G.fromList)
--          . filter ((>=2) . length)
--          . filter (\x -> nub x == x)
--          . shrink
--          . G.toList
--          . cuts

-- instance Arbitrary CutDirection where
--   arbitrary = elements [ CutLower
--                        , CutHigher
--                        ]


-- ----------------------------------------------------------------
-- -- Histogram instance
-- ----------------------------------------------------------------

-- instance (Bin bin, U.Unbox a, Arbitrary bin, Arbitrary a) => Arbitrary (Histogram bin a) where
--     arbitrary = do
--       bin <- suchThat arbitrary ((<333) . nBins)
--       histogramUO bin <$> arbitrary <*> (U.fromList <$> vectorOf (nBins bin) arbitrary)


-- ----------------------------------------------------------------
-- -- Arbitrary for bin values
-- ----------------------------------------------------------------

-- -- | It's difficult to generate values that will fall into allowed
-- --   range of the bin. Simple @inRange x ===> ...@ won't do because QC
-- --   will generate large and larger values and eventually will give up.
-- class ArbitraryBin bin where
--   -- | Generates arbitrary bin value that lies in range
--   arbitraryBinVal :: bin -> Gen (BinValue bin)

-- instance ArbitraryBin BinI where
--   arbitraryBinVal bin = choose (lowerLimit bin, lowerLimit bin)

-- instance (Enum e, Ord e) => ArbitraryBin (BinEnum e) where
--   arbitraryBinVal bin =
--     toEnum <$> choose (fromEnum $ lowerLimit bin, fromEnum $ lowerLimit bin)

-- instance (ArbitraryBin bX, ArbitraryBin bY) => ArbitraryBin (Bin2D bX bY) where
--   arbitraryBinVal (Bin2D bX bY) =
--     (,) <$> arbitraryBinVal bX <*> arbitraryBinVal bY

-- instance (Random a, Ord a, Fractional a, G.Vector v a) => ArbitraryBin (BinVarG v a) where
--   arbitraryBinVal bin = choose (lowerLimit bin, lowerLimit bin)
