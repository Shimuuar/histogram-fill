{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE Rank2Types #-}
module Data.Histogram.Fill ( -- * Typeclasses and existentials 
                             HBuilderCl(..)
                           , HBuilder
                           , builderList
                           , builderListWrap

                           -- * Fill routines
                           , createHistograms
                           -- * Histogram constructors 
                           , mkHist
                           , mkHist1

                           , HistBuilder

                           , module Data.Histogram.Bin
                           ) where

import Control.Arrow    (first)
import Control.Monad.ST (ST)
import Data.Ix          (Ix)
import Data.Monoid      (Monoid)

import Data.Histogram.Bin
import Data.Histogram.Internal.Accumulator
import Data.Histogram.Internal.Storage

----------------------------------------------------------------

-- | Create and fill histogram(s).
createHistograms :: Monoid b => HBuilder a b -> [a] -> b
createHistograms h xs = fillHistograms (runBuilder h) xs

----------------------------------------------------------------

-- | Histogram builder typeclass. Value of this type contain instructions
--   how to build histograms.
class HBuilderCl h where 
    -- | Convert input type of histogram from a to a'
    modifyIn  :: (a' -> a) -> h a b -> h a' b 
    -- | Convert output of histogram 
    modifyOut :: (b -> b') -> h a b -> h a  b'
    -- | Create stateful histogram from instructions
    runBuilder :: h a b -> HistogramST s a b


----------------------------------------------------------------

-- | Abstract histogram builder.
data HBuilder a b where
    MkHBuilder :: HBuilderCl h => h a b -> HBuilder a b

instance HBuilderCl HBuilder where 
    modifyIn  f (MkHBuilder h) = MkHBuilder $ modifyIn f h
    modifyOut g (MkHBuilder h) = MkHBuilder $ modifyOut g h 
    runBuilder  (MkHBuilder h) = runBuilder h


----------------------------------------------------------------

-- List of histograms. 
newtype HBuilderList a b = HBuilderList [HBuilder a b]

-- | Wrap list of histogram builders into HBuilder existential
builderList :: [HBuilder a b] -> HBuilder a b
builderList = MkHBuilder . HBuilderList

builderListWrap :: [HBuilder a b] -> HBuilder a [b]
builderListWrap = MkHBuilder . modifyOut (:[]) . HBuilderList

instance HBuilderCl HBuilderList where
    modifyIn  f (HBuilderList l) = HBuilderList $ map (modifyIn f) l
    modifyOut g (HBuilderList l) = HBuilderList $ map (modifyOut g) l
    runBuilder (HBuilderList l)  = accumList $ map runBuilder l

----------------------------------------------------------------

-- | Generic histogram builder. It's designed to be as general as possible. 
-- 
-- ix is supposed to be of Ix typeclass, v of Num.
data HistBuilder st a b where
    HistBuilder :: Storage st => (a -> Input st) -> (Output st -> b) -> (forall s . ST s (st s)) -> HistBuilder st a b

instance HBuilderCl (HistBuilder st) where
    modifyIn  f (HistBuilder inp out x) = HistBuilder (inp . f) out     x  
    modifyOut g (HistBuilder inp out x) = HistBuilder inp      (g .out) x  
    runBuilder  (HistBuilder inp out x) = do s <- x
                                             accumHist inp out s


----------------------------------------------------------------
-- Histogram constructors 
----------------------------------------------------------------

-- Convert from index to bin value
convert :: Bin bin => bin -> (a,[(BinIndex bin,a)],a) -> (a,[(BinValue bin,a)],a)
convert bin (u,xs,o) = (u, map (first $ fromIndex bin) xs, o)

-- | Create histogram builder which take single item as input. Each item has weight 1.
mkHist1 :: (Bin bin, Ix (BinIndex bin)) => 
           ( (Int, [(BinValue bin, Int)], Int) -> b)
        -> bin
        -> (a -> BinValue bin) 
        -> HBuilder a b
mkHist1 out bin inp = 
    let storage = newStorageUOne (getRange bin) (0 :: Int)
    in  MkHBuilder $ HistBuilder (toIndex bin . inp) (out . convert bin) storage

-- | Create histogram builder which take many items as input. Each item has weight 1.
mkHist :: (Bin bin, Ix (BinIndex bin)) => 
          ( (Int, [(BinValue bin, Int)], Int) -> b)
       -> bin
       -> (a -> [BinValue bin]) 
       -> HBuilder a b
mkHist out bin inp = 
    let storage = newStorageUMany (getRange bin) (0 :: Int)
    in  MkHBuilder $ HistBuilder (map (toIndex bin) . inp) (out . convert bin) storage
