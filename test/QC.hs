{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Rank2Types        #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
import Control.Applicative

import qualified Data.Vector.Unboxed as U

import Text.Printf
import Test.QuickCheck
import Test.QuickCheck.Property

import Data.Typeable
import Data.Histogram
import Data.Histogram.Bin.MaybeBin
import Data.Histogram.Fill

import Debug.Trace

----------------------------------------------------------------
-- Helpers
----------------------------------------------------------------

data T a = T

paramOfT :: T a -> a
paramOfT _ = undefined

typeOfT :: Typeable a => T a -> TypeRep
typeOfT = typeOf . paramOfT

report :: Typeable a => String -> T a -> IO ()
report str t = printf "==== %s for '%s' ====\n\n" str (show $ typeOfT t)

run :: (Typeable a, Testable prop) => (T a -> prop) -> T a -> IO ()
run prop t = do
  r <- quickCheckWithResult stdArgs (prop t)
  case r of 
    GaveUp            {} -> report "Gave up" t
    Failure           {} -> report "Failure" t
    NoExpectedFailure {} -> report "Unexpected failure" t
    Success           {} -> return () 

test :: Testable prop => String -> prop -> IO ()
test s prop = do
  r <- quickCheckWithResult stdArgs prop
  case r of 
    GaveUp            {} -> printf "---- Gave up: %s ----\n\n" s
    Failure           {} -> printf "---- Failure: %s\n\n" s
    NoExpectedFailure {} -> printf "---- Unexpected failure: %s ----\n\n" s
    Success           {} -> return () 

----------------------------------------------------------------
-- Arbitrary instances
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
      n    <- choose (1,10^3)
      return $ BinInt base step n

instance (Arbitrary a, Ord a, Enum a) => Arbitrary (BinEnum a) where
    arbitrary = do
      l <- arbitrary
      h <- suchThat arbitrary (>= l)
      return $ binEnum l h

instance Arbitrary (BinF Float) where
    arbitrary = do
      lo <- choose (-1.0e+3-1 , 1.0e+3)
      n  <- choose (1, 10^3)
      hi <- choose (lo , 1.0e+3+1)
      return $ binF lo n hi

instance Arbitrary (BinF Double) where
    arbitrary = do
      lo <- choose (-1.0e+6-1 , 1.0e+6)
      n  <- choose (1, 10^6)
      hi <- choose (lo , 1.0e+6+1)
      return $ binF lo n hi

instance Arbitrary BinD where
    arbitrary = do
      lo <- choose (-1.0e+6-1 , 1.0e+6)
      n  <- choose (1, 10^6)
      hi <- choose (lo , 1.0e+6+1)
      return $ binD lo n hi

instance Arbitrary LogBinD where
    arbitrary = do
      lo <- choose (1.0e-6 , 1.0e+6)
      n  <- choose (1, 10^6)
      hi <- choose (lo , 1.0e+6+1)
      return $ logBinD lo n hi

instance (Arbitrary bx, Arbitrary by) => Arbitrary (Bin2D bx by) where
    arbitrary = Bin2D <$> arbitrary <*> arbitrary

instance (Bin bin, U.Unbox a, Arbitrary bin, Arbitrary a) => Arbitrary (Histogram bin a) where
    arbitrary = do
      bin <- suchThat arbitrary ((<333) . nBins)
      histogramUO bin <$> arbitrary <*> (U.fromList <$> vectorOf (nBins bin) arbitrary)


----------------------------------------------------------------
-- Generic properties
----------------------------------------------------------------

isIdentity :: Eq a => (a -> a) -> a -> Bool
isIdentity f x = x == f x


----------------------------------------------------------------
-- Equality reflexivity
prop_Eq :: Eq a => T a -> a -> Bool
prop_Eq _ x = x == x

testsEq :: IO ()
testsEq = sequence_
  [ putStrLn "---- Equality reflexivity ----"
  , run prop_Eq (T :: T  BinI)
  , run prop_Eq (T :: T  BinInt)
  , run prop_Eq (T :: T (BinEnum Char))
  , run prop_Eq (T :: T (BinF Double))
  , run prop_Eq (T :: T (BinF Float))
  , run prop_Eq (T :: T  BinD)
  , run prop_Eq (T :: T  LogBinD)
  , run prop_Eq (T :: T (Bin2D BinI BinI))
  ]

----------------------------------------------------------------
-- > read . show == id
prop_ReadShow :: (Read a, Show a, Eq a) => T a -> a -> Bool
prop_ReadShow _ = isIdentity (read . show)

testsRead :: IO ()
testsRead = sequence_
  [ putStrLn "---- read . show == id ----"
  , run prop_ReadShow (T :: T BinI)
  , run prop_ReadShow (T :: T BinInt)
  , run prop_ReadShow (T :: T (BinEnum Char))
  , run prop_ReadShow (T :: T (BinF Double))
  , run prop_ReadShow (T :: T (BinF Float))
  , run prop_ReadShow (T :: T BinD)
  , run prop_ReadShow (T :: T LogBinD)
  , run prop_ReadShow (T :: T (Bin2D BinI BinI))
  ]


----------------------------------------------------------------
-- > toIndex . fromIndex == id
prop_ToFrom :: Bin bin => T bin -> Int -> bin -> Bool
prop_ToFrom _ i bin 
  | i >= 0 && i < nBins bin = isIdentity (toIndex bin . fromIndex bin) i
  | otherwise               = True  -- Equality doesn't hold for out of range indices

testsToFrom :: IO ()
testsToFrom = sequence_
  [ putStrLn "---- toIndex . fromIndex ----"
  , run prop_ToFrom (T :: T BinI)
  , run prop_ToFrom (T :: T BinInt)
  , run prop_ToFrom (T :: T (BinEnum Char))
  , run prop_ToFrom (T :: T (BinF Double))
  , run prop_ToFrom (T :: T (BinF Double))
  , run prop_ToFrom (T :: T BinD)
  , run prop_ToFrom (T :: T LogBinD)
  , run prop_ToFrom (T :: T (Bin2D BinI BinI))
  ]

----------------------------------------------------------------
-- > fromIndex . toIndex == id
-- Hold only for integral bins
prop_FromTo :: (Bin bin, Eq (BinValue bin)) => T bin -> BinValue bin -> bin -> Bool
prop_FromTo _ x bin 
  | inRange bin x = isIdentity (fromIndex bin . toIndex bin) x
  | otherwise     = True -- Doesn't hold for out of range indices

testsFromTo :: IO ()
testsFromTo = sequence_
  [ putStrLn "==== fromIndex . toIdex == id ===="
  , run prop_FromTo (T :: T BinI)
  , run prop_FromTo (T :: T (BinEnum Char))
  , run prop_FromTo (T :: T (Bin2D BinI BinI))
  ]


----------------------------------------------------------------
-- > inRange b x == indexInRange b x

indexInRange :: Bin b => b -> Int -> Bool
indexInRange b i = i >= 0  &&  i < nBins b

prop_InRange :: (Bin bin) => T bin -> bin -> BinValue bin -> Bool
prop_InRange _ b x = inRange b x == indexInRange b (toIndex b x)

testsInRange :: IO ()
testsInRange = sequence_
  [ putStrLn "---- inRange b x == indexInRange b x ----"
  , run prop_InRange (T :: T BinI)
  , run prop_InRange (T :: T BinInt)
  , run prop_InRange (T :: T (BinEnum Char))
  , run prop_InRange (T :: T (BinF Float))
  , run prop_InRange (T :: T (BinF Double))
  , run prop_InRange (T :: T BinD)
  , run prop_InRange (T :: T LogBinD)
  , run prop_InRange (T :: T (Bin2D BinI BinI))
  ]

----------------------------------------------------------------
-- Sliced bin hae correct number of bins

prop_sliceBin :: Bin1D b => T b -> b -> Gen Bool
prop_sliceBin _ bin = do
  let n = nBins bin
  i <- choose (0, n-1)
  j <- choose (i, n-1)
  return $ nBins (sliceBin i j bin) == (j - i + 1)

testsSliceBin :: IO ()
testsSliceBin = sequence_ 
  [ putStrLn "---- sliceBin ----"
  , run prop_sliceBin (T :: T BinI)
  , run prop_sliceBin (T :: T BinInt)
  , run prop_sliceBin (T :: T (BinF Float))
  , run prop_sliceBin (T :: T BinD)
  ] 
----------------------------------------------------------------
-- > fmap id == id
testsFMap :: IO ()
testsFMap = sequence_
  [ putStrLn "---- fmap preserves idenitity ----"
  , test "fmapBinX"    (isIdentity (fmapBinX   id) :: Bin2D BinI BinI    -> Bool)
  , test "fmapBinY"    (isIdentity (fmapBinY   id) :: Bin2D BinI BinI    -> Bool)
  , test "mapHist"     (isIdentity (histMap    id) :: Histogram BinI Int -> Bool)
  , test "mapHistBin"  (isIdentity (histMapBin id) :: Histogram BinI Int -> Bool)
  ]

----------------------------------------------------------------
-- Tests for histograms
prop_histSane :: (Bin bin, U.Unbox v) => T (Histogram bin v) -> Histogram bin v -> Bool
prop_histSane _ h = nBins (bins h) == U.length (histData h)

prop_sliceByIx :: (Bin1D bin, U.Unbox v) => T (Histogram bin v) -> Int -> Int -> Histogram bin v -> Property
prop_sliceByIx _ i j h =
  and [i >= 0, i < n, j >= 0, j < n, i <= j] ==> nBins (bins h') == U.length (histData h') 
    where
      n = nBins (bins h)
      h' = sliceByIx i j h

testsHistogram :: IO ()
testsHistogram = sequence_
  [ putStrLn "---- Test for histograms ----"
  , run prop_histSane t
  , run prop_Eq       t
  , test "read/show"  (isIdentity (readHistogram . show) :: Histogram BinI Int -> Bool)
  , test "sliceByIx" $ prop_sliceByIx t
  ]
  where t = T :: T (Histogram BinI Int)

----------------------------------------------------------------
-- Tests for filling

-- Test that empty histogram is filled with zeroes
prop_notfilled :: HBuilder i (Histogram BinI Int) -> Bool
prop_notfilled hb = 
  and [ outOfRange h == Just (0,0) 
      , U.all (==0) (histData h)
      , nBins (bins h) == U.length (histData h)
      ]
    where 
      h = fillBuilder hb []


testsFill :: IO ()
testsFill = sequence_
  [ putStrLn "---- Test for filling ----"
  , quickCheck (prop_notfilled . mkSimple  )
  , quickCheck (prop_notfilled . mkWeighted)
  ]


----------------------------------------------------------------
-- Main
----------------------------------------------------------------                
testsAll :: IO ()
testsAll = sequence_ [ testsEq
                     , testsRead
                     , testsToFrom
                     , testsFromTo
                     , testsInRange
                     , testsFMap
                     , testsFill
                     ]

main :: IO ()
main = testsAll
