{-# LANGUAGE FlexibleContexts  #-}
import Data.Typeable

import Test.QuickCheck
import Test.Tasty            (TestTree,testGroup,defaultMain)
import Test.Tasty.QuickCheck (testProperty)

import Data.Histogram
import Data.Histogram.Bin.MaybeBin
import QC.Instances



----------------------------------------------------------------
--
----------------------------------------------------------------
tests :: TestTree
tests = testGroup "tests"
  [ testGroup "Bins"
    [ testsBin (T :: T BinI)
    , testsBin (T :: T BinInt) 
    , testsBin (T :: T (BinF Float)) 
    , testsBin (T :: T (BinF Float))
    , testsBin (T :: T BinD)
    , testsBin (T :: T (BinEnum Char))
    , testsBin (T :: T LogBinD)
    , testsBin (T :: T (MaybeBin BinI))
    , testsBin (T :: T (Bin2D BinI BinI))
    ]
  , testGroup "fromIndex . toIndex == is" 
    [ testProperty "BinI"    $ prop_FromTo (T :: T BinI)
    , testProperty "BinEnum" $ prop_FromTo (T :: T (BinEnum Char))
    , testProperty "Bin2D"   $ prop_FromTo (T :: T (Bin2D BinI BinI))
    ]
  , testGroup "Sliceable bins"
    [ testSliceBin (T :: T BinI)
    , testSliceBin (T :: T BinInt) 
    , testSliceBin (T :: T (BinF Float)) 
    , testSliceBin (T :: T (BinF Float))
    , testSliceBin (T :: T BinD)
    , testSliceBin (T :: T (BinEnum Char))
    , testSliceBin (T :: T LogBinD)
    ]      
  , testGroup "Histogram"
    [ testProperty "read . show"  (isIdentity (readHistogram . show) :: Histogram BinI Int -> Bool)
    ]
  ]

testsBin :: ( Read a, Show a, Show (BinValue a), Eq a, Typeable a
            , Bin a
            , Arbitrary a, Arbitrary (BinValue a)
            ) => T a -> TestTree
testsBin t
  = testGroup ("Bin test for " ++ show (typeOfT t))
  [ testProperty "read . show = id"         $ prop_ReadShow t
  , testProperty "toIndex . fromIndex = id" $ prop_ToFrom   t
  , testProperty "inRange"                  $ prop_InRange  t
  ]

testSliceBin :: ( Show b, Typeable b, SliceableBin b, Arbitrary b
                ) => T b -> TestTree
testSliceBin t 
  = testGroup ("Slice tests for" ++ show (typeOfT t))
  [ testProperty "N of bins"  $ prop_sliceBin t
  ]


----------------------------------------------------------------
-- Bin tests
----------------------------------------------------------------

-- > read . show == id
prop_ReadShow :: (Read a, Show a, Eq a) => T a -> a -> Bool
prop_ReadShow _ = isIdentity (read . show)

-- > toIndex . fromIndex == id
prop_ToFrom :: Bin bin => T bin -> bin -> Gen Bool
prop_ToFrom _ bin = do
  i <- choose (0,nBins bin - 1)
  return $ isIdentity (toIndex bin . fromIndex bin) i

-- > fromIndex . toIndex == id
-- Hold only for integral bins
prop_FromTo :: (Bin bin, Eq (BinValue bin), ArbitraryBin bin)
            => T bin -> bin -> Gen Bool
prop_FromTo _ bin = do
  x <- arbitraryBinVal bin
  return $ isIdentity (fromIndex bin . toIndex bin) x

-- > inRange b x == indexInRange b x
prop_InRange :: (Bin bin) => T bin -> bin -> BinValue bin -> Bool
prop_InRange _ b x 
  = inRange b x == indexInRange (toIndex b x)
  where
    indexInRange i = i >= 0  &&  i < nBins b

-- Sliced bin have correct number of bins
prop_sliceBin :: (SliceableBin b) => T b -> b -> Gen Bool
prop_sliceBin _ bin = do
  let n = nBins bin
  i <- choose (0, n-1)
  j <- choose (i, n-1)
  return $ nBins (sliceBin i j bin) == (j - i + 1)



----------------------------------------------------------------
-- Helpers
----------------------------------------------------------------

isIdentity :: Eq a => (a -> a) -> a -> Bool
isIdentity f x = x == f x

data T a = T

paramOfT :: T a -> a
paramOfT _ = undefined

typeOfT :: Typeable a => T a -> TypeRep
typeOfT = typeOf . paramOfT

----------------------------------------------------------------
-- Main
----------------------------------------------------------------

main :: IO ()
main =
  defaultMain tests