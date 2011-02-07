{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Rank2Types        #-}

{-# LANGUAGE TypeSynonymInstances #-}
import Control.Applicative
import Control.Monad
import Control.Monad.ST

import qualified Data.Vector.Unboxed as U

import Test.QuickCheck
import System.Random

import Data.Histogram
import Data.Histogram.Fill
import Data.Histogram.Bin

import Debug.Trace

----------------------------------------------------------------
-- Helpers
----------------------------------------------------------------

test :: Testable prop => String -> prop -> (String, IO ())
test s prop = (s, quickCheck prop)

title :: String -> (String,IO ())
title s = (s, return ())

runTests :: [(String, IO ())] -> IO ()
runTests = mapM_ $ \(name, test) -> putStrLn (" * " ++ name) >> test



----------------------------------------------------------------
-- Arbitrary instances
----------------------------------------------------------------

instance Arbitrary BinI where
    arbitrary = do
      let maxI = 100
      lo <- choose (-maxI , maxI)
      hi <- choose (lo    , maxI)
      return $ BinI lo hi

instance Arbitrary BinInt where
    arbitrary = do
      let maxI = 100
      base <- choose (-maxI,maxI)
      step <- choose (1,10)
      max  <- choose (base, base+2*maxI)
      return $ binInt base step max

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

-- Test that function is identity
isIdentity :: Eq a => (a -> a) -> a -> Bool
isIdentity f x = x == f x



----------------------------------------------------------------

-- Equality reflexivity
prop_Eq :: Eq a => a -> Bool
prop_Eq x = x == x

testsEq :: [(String, IO ())]
testsEq = [ title "==== Equality reflexivity ===="
          , test "BinI"            (prop_Eq :: BinI            -> Bool)
          , test "BinInt"          (prop_Eq :: BinInt          -> Bool)
          , test "BinEnum Char"    (prop_Eq :: BinEnum Char    -> Bool)
          , test "BinF Double"     (prop_Eq :: BinF Double     -> Bool)
          , test "BinF Float"      (prop_Eq :: BinF Float      -> Bool)
          , test "BinD"            (prop_Eq :: BinD            -> Bool)
          , test "LogBinD"         (prop_Eq :: LogBinD         -> Bool)
          , test "Bin2D BinI Bini" (prop_Eq :: Bin2D BinI BinI -> Bool)
          ]


-- read . show == id
prop_ReadShow :: (Read a, Show a, Eq a) => a -> Bool
prop_ReadShow = isIdentity (read . show)

testsRead :: [(String, IO ())]
testsRead = [ title "==== read . show == id ===="
            , test "BinI"            (prop_ReadShow  :: BinI            -> Bool)
            , test "BinInt"          (prop_ReadShow  :: BinInt          -> Bool)
            , test "BinEnum Char"    (prop_ReadShow  :: BinEnum Char    -> Bool)
            , test "BinF Double"     (prop_ReadShow  :: BinF Double     -> Bool)
            , test "BinF Float"      (prop_ReadShow  :: BinF Float      -> Bool)
            , test "BinD"            (prop_ReadShow  :: BinD            -> Bool)
            , test "LogBinD"         (prop_ReadShow  :: LogBinD         -> Bool)
            , test "Bin2D BinI BinI" (prop_ReadShow  :: Bin2D BinI BinI -> Bool)
            ]


-- toIndex . fromIndex == id
prop_ToFrom :: (Bin bin) => Int -> bin -> Bool
prop_ToFrom x bin | inRange bin val = x == toIndex bin val
                  | otherwise       = True -- Equality doesn't hold for out of range indices
                    where val = fromIndex bin x

testsToFrom :: [(String,IO())]
testsToFrom = [ title "==== toIndex . fromIndex == id"
              , test "BinI"            (prop_ToFrom :: Int -> BinI            -> Bool)
              -- , test "BinEnum Char"    (prop_ToFrom :: Int -> BinEnum Char    -> Bool)
              , test "BinInt"          (prop_ToFrom :: Int -> BinInt          -> Bool)
              , test "BinF Double"     (prop_ToFrom :: Int -> BinF Double     -> Bool)
              , test "BinD"            (prop_ToFrom :: Int -> BinD            -> Bool)
              , test "LogBinD"         (prop_ToFrom :: Int -> LogBinD         -> Bool)
              , test "Bin2D BinI BinI" (prop_ToFrom :: Int -> Bin2D BinI BinI -> Bool)
              ]



-- fromIndex . toIndex == id
-- Hold only for integral bins
prop_FromTo :: (Bin bin, Eq (BinValue bin)) => BinValue bin -> bin -> Bool
prop_FromTo x bin | inRange bin x = isIdentity (fromIndex bin . toIndex bin) x
                  | otherwise     = True -- Doesn't hold for out of range indices

testsFromTo :: [(String, IO ())]
testsFromTo = [ title "==== fromIndex . toIdex == id ===="
              , test "BinI"            (prop_FromTo :: Int       -> BinI            -> Bool)
              -- , test "BinEnum Char"    (prop_FromTo :: Char      -> BinEnum Char    -> Bool)
              , test "Bin2D BinI BinI" (prop_FromTo :: (Int,Int) -> Bin2D BinI BinI -> Bool)
              ]


indexInRange :: Bin b => b -> Int -> Bool
indexInRange b i = i >= 0  &&  i < nBins b

-- inRange b x == indexInRange b x
type Prop_InRange bin = bin -> BinValue bin -> Bool
prop_InRange :: (Bin bin) => Prop_InRange bin 
prop_InRange b x = inRange b x == indexInRange b (toIndex b x)

testsInRange :: [(String,IO())]
testsInRange = [ title "==== inRange b x == indexInRange b x ===="
               , test "BinI"            (prop_InRange :: Prop_InRange BinI)
               , test "BinInt"          (prop_InRange :: Prop_InRange BinInt)
               -- , test "BinEnum Char"    (prop_InRange :: Prop_InRange (BinEnum Char))
               , test "BinF Float"      (prop_InRange :: Prop_InRange (BinF Float))
               , test "BinF Double"     (prop_InRange :: Prop_InRange (BinF Double))
               , test "BinD"            (prop_InRange :: Prop_InRange BinD)
               , test "LogBinD"         (prop_InRange :: Prop_InRange LogBinD)
               , test "Bin2D BinI BinI" (prop_InRange :: Prop_InRange (Bin2D BinI BinI))
               ]

testsFMap :: [(String, IO ())]
testsFMap = [ title "==== fmap preserves idenitity ===="
            , test "fmapBinX"    (isIdentity (fmapBinX   id) :: Bin2D BinI BinI    -> Bool)
            , test "fmapBinY"    (isIdentity (fmapBinY   id) :: Bin2D BinI BinI    -> Bool)
            , test "mapHist"     (isIdentity (histMap    id) :: Histogram BinI Int -> Bool)
            , test "mapHistBin"  (isIdentity (histMapBin id) :: Histogram BinI Int -> Bool)
            ]


testsHistogram :: [(String, IO ())]
testsHistogram =
    [ title "==== Test for histograms ===="
    , test "equality"   (prop_Eq :: Histogram BinI Int -> Bool)
    , test "read/show"  (isIdentity (readHistogram . show) :: Histogram BinI Int -> Bool)
    ]


testsFill :: [(String, IO ())]
testsFill = [ title "==== Test for filling ===="
            -- Zeroness
            , test "zeroness mkSimple"   (prop_zeroness . mkSimple  )
            , test "zeroness mkWeighted" (prop_zeroness . mkWeighted)
            -- Sizes match
            , test "size mkHist"    (prop_size . mkSimple  )
            , test "size mkHistWgh" (prop_size . mkWeighted)
            ]
    where
      -- Test that empty histogram is filled with zeroes
      prop_zeroness :: HBuilder i (Histogram BinI Int) -> Bool
      prop_zeroness hb = outOfRange h == Just (0,0) && (U.all (==0) (histData h))
          where h = fillBuilder hb []
      -- Test that array size and bin sizes match
      prop_size :: HBuilder i (Histogram BinI Int) -> Bool
      prop_size hb = nBins (bins h) == U.length (histData h)
          where h = fillBuilder hb []


testsAll :: [(String, IO ())]
testsAll = concat [ testsEq
                  , testsRead
                  , testsToFrom
                  , testsFromTo
                  , testsInRange
                  , testsFMap
                  , testsFill
                  ]

main :: IO ()
main = do
  runTests testsAll
