{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Histogram.Bin.BinVar (
    BinVar(..)
  , unsafeBinVar
  , binVar
  , cuts
  , deleteCut
  , addCut
  , fromBin1D
  ) where

import           Control.DeepSeq (NFData(..))
import           Data.Data (Data,Typeable)
import           Data.Maybe
import qualified Data.Vector.Generic as G
import Data.Vector.Generic  (Vector,(!))

import           Data.Histogram.Bin.Classes

{-| bins of variable size.
   Bins are defined by a vector of `cuts` marking the boundary between bins. This assumes that the entire range is continuous.  There are n+1 cuts for n bins. This also implies that cuts are in ascending order.

-}
newtype BinVar v a = BinVar { _cuts :: v a } -- vector of cuts
                     deriving (Data,Typeable,Eq)

-- | Create variable bins unsafely
unsafeBinVar :: v a -- ^ cuts
             -> BinVar v a
unsafeBinVar = BinVar

-- | Create variable bins unsafely
binVar :: (Vector v (a,a), Vector v a, Ord a) 
       => v a -- ^ cuts
       -> BinVar v a
binVar c
    | G.length c < 2 = error "Data.Histogram.Bin.BinVar.binVar': nonpositive number of bins"
    | G.any (uncurry (>)) (G.zip (G.init c) (G.drop 1 c)) = error "Data.Histogram.Bin.BinVar.binVar': cuts not in ascending order"
    | otherwise = BinVar c

-- | access cuts
cuts :: BinVar v a -> v a
cuts (BinVar c) = c

instance (Vector v a, Num a, Ord a, Fractional a) => Bin (BinVar v a) where
  type BinValue (BinVar v a) = a
  toIndex (BinVar c) !x = case G.findIndex (>x) c of
      Nothing -> error "Data.Histogram.Bin.BinVar.toIndex: above range"
      Just i  -> case i of
          0 -> error "Data.Histogram.Bin.BinVar.toIndex: below range"
          _ -> i-1

  fromIndex (BinVar c) !i
      | i >= G.length c - 1 = 
            error "Data.Histogram.Bin.BinVar.fromIndex: above range"
      | otherwise = ((c ! i) + (c ! (i+1)))/2

  nBins (BinVar c) = if G.length c < 2 then 0 else G.length c - 1
  {-# INLINE toIndex #-}

instance (Vector v a, Num a, Ord a, Fractional a) => IntervalBin (BinVar v a) where
  binInterval (BinVar c) i = (c ! i, c ! (i+1))

instance (Vector v a, Num a, Ord a, Fractional a) => Bin1D (BinVar v a) where
  lowerLimit (BinVar c) = G.head c
  upperLimit (BinVar c) = G.last c

instance (Vector v a, Num a, Ord a, Fractional a) => SliceableBin (BinVar v a) where
  unsafeSliceBin i j (BinVar c) = BinVar (G.drop i $ G.take (j-i) c)

instance (Vector v a, Num a, Ord a, Fractional a) => VariableBin (BinVar v a) where
  binSizeN (BinVar c) !i = c ! (i+1) - c ! i

-- | Equality is up to 3e-11 (2/3th of digits)
instance (Vector v (a,a), Vector v a, Num a, Ord a, Fractional a) => BinEq (BinVar v a) where
  binEq (BinVar c) (BinVar c')
    =  isNothing (G.find (\(d,d') -> d - d' > eps * abs d) (G.zip c c'))
    where
      eps = 3e-11

instance (Vector v a, Show a, Num a, Ord a, Fractional a) => Show (BinVar v a) where
  show (BinVar c) = "# BinVar cuts\n" ++ concat (fmap showCut (G.toList c)) ++ "\n\n"
    where
      showCut x = show x ++ "\t"

instance (NFData (v a)) => NFData (BinVar v a) where
   rnf (BinVar c) =
     rnf c `seq` ()

-- | delete a cut, which effectively reduces the entire range of the bins (if the cut was the first or last one) or merges two bins (if the cut was in the middle)
deleteCut :: (Vector v a, Num a, Ord a, Fractional a) 
          => BinVar v a -- bin 
          -> Int        -- cut index 
          -> BinVar v a
deleteCut (BinVar c) !i
  | G.length c <= 2 = 
    error "Data.Histogram.Bin.BinVar.deleteCut: deleting cut but 2 or less cuts"
  | otherwise = BinVar (G.take i c G.++ G.drop (i+1) c)

-- | insert a new cut which effectively extends the range of the bins or splits a bin
addCut :: (Vector v a, Num a, Ord a, Fractional a) 
       => BinVar v a -- bin 
       -> a          -- new cut value
       -> BinVar v a
addCut (BinVar c) !x = BinVar (G.concat [G.take i c, G.singleton x, G.drop i c])
  where
    i = fromMaybe (G.length c) (G.findIndex (> x) c)

fromBin1D :: (IntervalBin b, Vector v (BinValue b, BinValue b),Vector v (BinValue b)) => b -> BinVar v (BinValue b)
fromBin1D b = 
  let buckets = binsList b in
  binVar (G.init (G.map fst buckets) `G.snoc` G.last (G.map snd buckets))

