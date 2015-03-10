{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Data.Histogram.Bin.BinVar (
    BinVarG(..)
  , BinVar
  , unsafeBinVar
  , binVar
  , cuts
  , deleteCut
  , addCut
  ) where

import           Control.DeepSeq (NFData(..))
import           Data.Typeable
import           Data.Maybe
import qualified Data.Vector.Generic as G
import           Data.Vector.Generic  (Vector,(!))
import qualified Data.Vector.Fusion.Stream as S
import qualified Data.Vector.Unboxed       as U
import           Text.Read       (Read(..))

import           Data.Histogram.Bin.Classes
import           Data.Histogram.Bin.Read



-- | Bins of variable size. Bins are defined by a vector of `cuts`
--   marking the boundary between bins. This assumes that the entire
--   range is continuous.  There are n+1 cuts for n bins. This also
--   implies that cuts are in ascending order.
newtype BinVarG v a = BinVarG { _cuts :: v a } -- vector of cuts
                    deriving (Eq
#if MIN_VERSION_base(4,7,0)
                              , Typeable
#endif
                              )

-- | Type synonym for @BinVarG@ specialized for unboxed vectors
type BinVar = BinVarG U.Vector

#if !MIN_VERSION_base(4,7,0)
histTyCon :: String -> String -> TyCon
#if MIN_VERSION_base(4,4,0)
histTyCon = mkTyCon3 "histogram-fill"
#else
histTyCon m s = mkTyCon $ m ++ "." ++ s
#endif
-- end MIN_VERSION_base(4,4,0)
instance Typeable1 v => Typeable1 (BinVarG v) where
  typeOf1 b = mkTyConApp (histTyCon "Data.Histogram.Bin.BinVar" "BinVarG")
                         [typeOf1 $ cuts b]
#endif

-- | Create variable bins unsafely
unsafeBinVar :: v a -- ^ cuts
             -> BinVarG v a
unsafeBinVar = BinVarG

-- | Create variable bins unsafely
binVar :: (Vector v a, Ord a)
       => v a -- ^ cuts
       -> BinVarG v a
binVar c
  | G.length c < 2
  = error "Data.Histogram.Bin.BinVar.binVar': nonpositive number of bins"
  | S.or $ S.zipWith (>=) (G.stream c) (G.stream (G.tail c))
  = error "Data.Histogram.Bin.BinVar.binVar': cuts not in ascending order"
  | otherwise = BinVarG c

-- | access cuts
cuts :: BinVarG v a -> v a
cuts (BinVarG c) = c

instance (Vector v a, Ord a, Fractional a) => Bin (BinVarG v a) where
  type BinValue (BinVarG v a) = a
  toIndex (BinVarG c) !x = case G.findIndex (>x) c of
      Nothing -> G.length c - 1
      Just i  -> case i of
          0 -> -1
          _ -> i-1

  -- FIXME: We use fractional here but it means that we cannot use it for Int!
  fromIndex (BinVarG c) !i
      | i >= G.length c - 1 =
            error "Data.Histogram.Bin.BinVar.fromIndex: above range"
      | otherwise = ((c ! i) + (c ! (i+1)))/2
  nBins (BinVarG c) = if G.length c < 2 then 0 else G.length c - 1
  {-# INLINE toIndex #-}

instance (Vector v a, Ord a, Fractional a) => IntervalBin (BinVarG v a) where
  binInterval (BinVarG c) i = (c ! i, c ! (i+1))

instance (Vector v a, Ord a, Fractional a) => Bin1D (BinVarG v a) where
  lowerLimit (BinVarG c) = G.head c
  upperLimit (BinVarG c) = G.last c

instance (Vector v a, Ord a, Fractional a) => SliceableBin (BinVarG v a) where
  unsafeSliceBin i j (BinVarG c) = BinVarG (G.drop i $ G.take (j-i) c)

instance (Vector v a, Ord a, Fractional a) => VariableBin (BinVarG v a) where
  binSizeN (BinVarG c) !i = c ! (i+1) - c ! i

-- | Equality is up to 3e-11 (2/3th of digits)
instance (Vector v a, Ord a, Fractional a) => BinEq (BinVarG v a) where
  binEq (BinVarG c) (BinVarG c')
    =  (G.length c == G.length c')
    && (S.and (S.zipWith eq (G.stream c) (G.stream c')))
    where
      eq x y = abs (x - y) < eps * (abs x `max` abs y)
      eps    = 3e-11

instance (Vector v a, Show a, Fractional a) => Show (BinVarG v a) where
  show (BinVarG c) = "# BinVar\n# cuts = " ++ show (G.toList c) ++ "\n"

instance (Vector v a, Read a, Ord a, Fractional a) => Read (BinVarG v a) where
  readPrec = do keyword "BinVar"
                xs <- value "cuts"
                return $ binVar $ G.fromList xs


instance (NFData (v a)) => NFData (BinVarG v a) where
   rnf (BinVarG c) =
     rnf c `seq` ()

-- | Delete a cut, which effectively reduces the entire range of the
--   bins (if the cut was the first or last one) or merges two bins
--   (if the cut was in the middle)
deleteCut :: (Vector v a, Ord a, Fractional a)
          => BinVarG v a -- bin
          -> Int        -- cut index
          -> BinVarG v a
deleteCut (BinVarG c) !i
  | G.length c <= 2 =
    error "Data.Histogram.Bin.BinVar.deleteCut: deleting cut but 2 or less cuts"
  | otherwise = BinVarG (G.take i c G.++ G.drop (i+1) c)

-- | insert a new cut which effectively extends the range of the bins or splits a bin
addCut :: (Vector v a, Ord a, Fractional a)
       => BinVarG v a -- bin
       -> a          -- new cut value
       -> BinVarG v a
addCut (BinVarG c) !x = BinVarG (G.concat [G.take i c, G.singleton x, G.drop i c])
  where
    i = fromMaybe (G.length c) (G.findIndex (> x) c)

instance ( Bin1D b
         , Vector v (BinValue b)
         , a ~ (BinValue b)
         , Fractional a)
         => ConvertBin b (BinVarG v a) where
  convertBin b
    = binVar
    $ lowerLimit b `G.cons` G.generate (nBins b) (snd . binInterval b)
