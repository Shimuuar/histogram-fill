{-# LANGUAGE ScopedTypeVariables #-}
module Test.Binary ( tests
  ) where

import Data.Typeable
import Data.Binary

import Test.QuickCheck
import Test.Tasty            (TestTree,testGroup)
import Test.Tasty.QuickCheck (testProperty)

import Data.Histogram
import Data.Histogram.Bin.MaybeBin
import Data.Histogram.QuickCheck   ()
import Data.Histogram.Binary       ()


tests :: TestTree
tests = testGroup "cereal"
  [ testTagged p_serialize (T :: T BinI)
  , testTagged p_serialize (T :: T BinInt)
  , testTagged p_serialize (T :: T (BinF Float))
  , testTagged p_serialize (T :: T BinD)
  , testTagged p_serialize (T :: T (BinEnum Char))
  , testTagged p_serialize (T :: T LogBinD)
  , testTagged p_serialize (T :: T (MaybeBin BinI))
  , testTagged p_serialize (T :: T (Bin2D BinI BinI))
    --
  , testTagged p_serialize (T :: T (Histogram BinI Int))
  ]



p_serialize :: (Binary a, Eq a) => T a -> a -> Bool
p_serialize _ a = a == (decode . encode) a

data T a = T

testTagged :: forall a b. (Testable b, Typeable a) => (T a -> b) -> T a -> TestTree
testTagged prop t
  = testProperty (show $ typeOf (undefined :: a)) (prop t)
