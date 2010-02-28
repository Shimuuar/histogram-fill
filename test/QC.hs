{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
import Control.Applicative

import qualified Data.Vector.Unboxed as U

import Test.QuickCheck
import System.Random

import Data.Histogram
import Data.Histogram.Bin

----------------------------------------------------------------
-- Helpers

equalTest :: Eq a => (a -> a) -> a -> Bool
equalTest f x = x == f x

p :: Testable prop => prop -> IO ()
p = quickCheck

runTests :: [(String, IO ())] -> IO ()
runTests = mapM_ $ \(name, test) -> putStrLn (" * " ++ name) >> test

type Index = Int

----------------------------------------------------------------
-- Arbitrary Instance for BinI
instance Arbitrary BinI where
    arbitrary = do
      let maxI = 15000
      lo <- choose (-maxI , maxI)
      hi <- choose (lo    , maxI)
      return $ BinI lo hi

instance Arbitrary (BinIx a) where
    arbitrary = BinIx <$> arbitrary

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

instance (Arbitrary bx, Arbitrary by) => Arbitrary (Bin2D bx by) where
    arbitrary = Bin2D <$> arbitrary <*> arbitrary

instance (Bin bin, U.Unbox a, Arbitrary bin, Arbitrary a) => Arbitrary (Histogram bin a) where
    arbitrary = do
      bin <- arbitrary
      Histogram bin <$> arbitrary <*> (U.fromList <$> vectorOf (nBins bin) arbitrary)
----------------------------------------------------------------
-- Generic tests

-- equality reflexivity
eqTest :: Eq a => a -> Bool
eqTest x = x == x

-- read . show == id
readShowTest :: (Read a, Show a, Eq a) => a -> Bool
readShowTest = equalTest (read . show) 

readShowTestH :: (Read a, Show a, Eq a, Read bin, Show bin, Eq bin, Bin bin, U.Unbox a, Show (BinValue bin)) =>
                 Histogram bin a -> Bool
readShowTestH = equalTest (readHistogram . show) 

-- toIndex . fromIndex
fromToIndexTest :: (Bin bin) => (Index, bin) -> Bool
fromToIndexTest (x, bin) | inRange bin val = x == toIndex bin val 
                         | otherwise       = True -- Equality doesn't hold for out of range indices
                         where val = fromIndex bin x 

-- fromIndex . toIndex // Hold only for integral bins
toFromIndexTest :: (Bin bin, Eq (BinValue bin)) => (BinValue bin, bin) -> Bool
toFromIndexTest (x, bin) | inRange bin x = equalTest (fromIndex bin . toIndex bin) x
                         | otherwise     = True -- Doesn't hold for out of range indices
----------------------------------------------------------------

testsEq :: [(String, IO ())]
testsEq = [ ( "==== Equality reflexivity tests ====" , return ())
          , ( "BinI"        , p (eqTest :: BinI            -> Bool))
          , ( "BinIx Int"   , p (eqTest :: BinIx Int       -> Bool))
          , ( "BinF Double" , p (eqTest :: BinF Double     -> Bool))
          , ( "BinF Float"  , p (eqTest :: BinF Float      -> Bool))
          , ( "Bin2D"       , p (eqTest :: Bin2D BinI BinI -> Bool))
          , ( "Histogram"   , p (eqTest :: Histogram BinI Int -> Bool))
          ]
testsRead :: [(String, IO ())]
testsRead = [ ( "==== Read/Show tests ====" , return ())
            , ( "BinI"        , p (readShowTest  :: BinI            -> Bool))
            , ( "BinIx Int"   , p (readShowTest  :: BinIx Int       -> Bool))
            , ( "BinF Double" , p (readShowTest  :: BinF Double     -> Bool))
            , ( "BinF Float"  , p (readShowTest  :: BinF Float      -> Bool))
            , ( "Histogram"   , p (readShowTestH :: Histogram BinI        Int    -> Bool))
            ]
testsIndexing :: [(String, IO ())]
testsIndexing = [ ( "==== Bin {to,from}Index tests ====", return ())
                -- Integral bins
                , ( "BinI"        , p (fromToIndexTest :: (Index, BinI)        -> Bool))
                , ( "BinI'"       , p (toFromIndexTest :: (Int,   BinI)        -> Bool))
                , ( "BinIx"       , p (fromToIndexTest :: (Index, BinIx Int)   -> Bool))
                , ( "BinIx'"      , p (toFromIndexTest :: (Int,   BinIx Int)   -> Bool))
                -- Floating point bins
                -- No test for Float because of roundoff errors
                , ( "BinF Double" , p (fromToIndexTest :: (Index, BinF Double) -> Bool))
                -- 2D bins
                , ( "Bin2D"       , p (fromToIndexTest :: (Index, Bin2D BinI BinI) -> Bool))
                , ( "Bin2D"       , p (toFromIndexTest :: ((Int,Int), Bin2D BinI BinI) -> Bool))
                ]
testsFMap :: [(String, IO ())]
testsFMap = [ ("==== Tests for functor like functions ====", return ())
            , ("fmapBinX"    , p (equalTest (fmapBinX id) :: Bin2D BinI BinI -> Bool))
            , ("fmapBinY"    , p (equalTest (fmapBinY id) :: Bin2D BinI BinI -> Bool))
            , ("mapHist"     , p (equalTest (mapHist  id) :: Histogram BinI Int -> Bool))
            , ("mapHistBin"  , p (equalTest (mapHist  id) :: Histogram BinI Int -> Bool))
            , ("mapHistData" , p (equalTest (mapHist  id) :: Histogram BinI Int -> Bool))
            ]
testsAll :: [(String, IO ())]
testsAll = concat [ testsEq , testsRead , testsIndexing , testsFMap ]

main :: IO ()
main = do
  runTests testsAll
 