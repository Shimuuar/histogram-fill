{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
module Data.Histogram ( -- * Immutable histogram
                        Histogram(..)
                      , module Data.Histogram.Bin
                      , mapHist
                      , histBin
                      , histData
                      , underflows
                      , overflows
                      , outOfRange
                      -- * Conversion
                      , asList
                      , asPairVector
                      , asVectorPairs
                      -- * Slicing
                      , sliceY
                      , sliceX
                      ) where

import Control.Monad (unless)
import Data.Array.Vector
import Text.Read
import Text.ParserCombinators.ReadP    (many, satisfy)
import Text.ParserCombinators.ReadPrec (lift)

import Data.Histogram.Bin
import Data.Histogram.Parse


-- | Immutable histogram
data Histogram bin a where
    Histogram :: (Bin bin, UA a) => 
                 bin
              -> (a,a)
              -> UArr a
              -> Histogram bin a

instance (Show a, Show (BinValue bin), Show bin) => Show (Histogram bin a) where
    show h@(Histogram bin (u,o) _) = "# Histogram\n" ++
                                     "# Underflows = " ++ show u ++ "\n" ++
                                     "# Overflows  = " ++ show o ++ "\n" ++
                                     show bin ++
                                     (unlines $ map showT $ asList h)
        where
          showT (x,y) = show x ++ "\t" ++ show y

instance (Read a, Num a, UA a, Read bin, Bin bin) => Read (Histogram bin a) where
    readPrec = do
      keyword "Histogram"
      u   <- value "Underflows"
      o   <- value "Overflows"
      bin <- readPrec
      xs  <- (map last . filter (not . null) . map words . lines) `fmap` look
      -- Devour everything
      lift $ many $ satisfy (const True)
      rest <- look
      unless (null rest) $ pfail 
      -- Done
      return $ Histogram bin (u,o) (toU $ map read xs)

-- | fmap lookalike. 
mapHist :: UA b => (a -> b) -> Histogram bin a -> Histogram bin b
mapHist f (Histogram bin (u,o) a) = Histogram bin (f u, f o) (mapU f a)

-- | Histogram bins
histBin :: Histogram bin a -> bin
histBin (Histogram bin _ _) = bin

-- | Histogram data as vector
histData :: Histogram bin a -> UArr a
histData (Histogram _ _ a) = a

-- | Number of underflows
underflows :: Histogram bin a -> a
underflows (Histogram _ (u,_) _) = u

-- | Number of overflows
overflows :: Histogram bin a -> a
overflows (Histogram _ (u,_) _) = u

-- | Under and overflows
outOfRange :: Histogram bin a -> (a,a)
outOfRange (Histogram _ r _) = r


-- | Convert histogram to list
asList :: Histogram bin a -> [(BinValue bin, a)]
asList (Histogram bin _ arr) = map (fromIndex bin) [0..] `zip` fromU arr

-- | Convert to pair of vectors
asPairVector :: UA (BinValue bin) => Histogram bin a -> (UArr (BinValue bin), UArr a)
asPairVector (Histogram bin _ a) = (toU $ map (fromIndex bin) [0 .. nBins bin], a)

-- | Convert to vector of pairs
asVectorPairs :: UA (BinValue bin) => Histogram bin a -> UArr ((BinValue bin) :*: a)
asVectorPairs h@(Histogram _ _ _) = uncurry zipU . asPairVector $ h

-- FIXME: range must be discarded.
-- | Slice 2D histogram along Y axis
sliceY :: (Bin bX, Bin bY) => Histogram (Bin2D bX bY) a -> [(BinValue bY, Histogram bX a)]
sliceY (Histogram b@(Bin2D bX _) r a) = map mkHist $ init [0, nBins bX .. nBins b]
    where
      mkHist i = ( snd $ fromIndex b i
                 , Histogram bX r (sliceU a i (nBins bX)) )

-- | Slice 2D histogram along X axis
sliceX :: (Bin bX, Bin bY) => Histogram (Bin2D bX bY) a -> [(BinValue bX, Histogram bY a)]
sliceX (Histogram b@(Bin2D bX bY) r a) = map mkHist $ init [0 .. nx]
    where
      nx = nBins bX
      n  = nBins b
      mkHist i = ( fst $ fromIndex b i
                 , Histogram bY r (toU $ map (indexU a) [i,i+nx .. n-1]) )
