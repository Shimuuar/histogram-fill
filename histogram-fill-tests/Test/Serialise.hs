{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
-- |
module Test.Serialise where

import Data.Typeable
import Codec.Serialise

import Test.QuickCheck
import Test.Tasty            (TestTree,testGroup)
import Test.Tasty.QuickCheck (testProperty)

import Data.Histogram
import Data.Histogram.Bin.MaybeBin
import Data.Histogram.QuickCheck   ()
import Data.Histogram.Serialise    ()


tests :: TestTree
tests = testGroup "cereal"
  [ testSerialise @BinI
  , testSerialise @BinInt
  , testSerialise @(BinF Float)
  , testSerialise @BinD
  , testSerialise @(BinEnum Char)
  , testSerialise @LogBinD
  , testSerialise @(MaybeBin BinI)
  , testSerialise @(Bin2D BinI BinI)
    --
  , testSerialise @(Histogram BinI Int)
  ]


testSerialise :: forall a. (Typeable a, Serialise a, Eq a, Arbitrary a, Show a) => TestTree
testSerialise
  = testProperty (show $ typeOf (undefined :: a))
  $ \(a :: a) -> a == (deserialise . serialise) a
