{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE Rank2Types #-}
-- |
-- Module     : Data.Histogram.Fill
-- Copyright  : Copyright (c) 2009, Alexey Khudyakov <alexey.skladnoy@gmail.com>
-- License    : BSD3
-- Maintainer : Alexey Khudyakov <alexey.skladnoy@gmail.com>
-- Stability  : experimental
-- 
module Data.Histogram.Fill ( -- * Type classes & wrappers
                             HBuilderCl(..)
                           , HBuilder
                           , builderList
                           , builderListWrap

                           -- * Fill routines
                           , createHistograms

                           -- * Histogram constructors 
                           , module Data.Histogram.Bin
                           , mkHist
                           , mkHist1
                           , mkHistWgh
                           , mkHistWgh1
                           , mkHistList
                           , mkHistList1
                           -- * Internals
                           , HistBuilder
                           ) where

import Control.Arrow    (first)
import Control.Monad.ST (ST)
import Data.Monoid      (Monoid)

import Data.Histogram.Bin
import Data.Histogram.Internal.Accumulator
import Data.Histogram.Internal.Storage

----------------------------------------------------------------

-- | Create and fill histogram(s).
createHistograms :: Monoid b => HBuilder a b -> [a] -> b
createHistograms h xs = fillHistograms (runBuilder h) xs

----------------------------------------------------------------

-- | Histogram builder typeclass. Instance of this class contain
--   instructions how to build histograms.
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

-- | Wrap list of histogram builders into HBuilder.
builderList :: [HBuilder a b] -> HBuilder a b
builderList = MkHBuilder . HBuilderList

-- | Wrap list of histogram builders into HBuilder and change they return type 
builderListWrap :: [HBuilder a b] -> HBuilder a [b]
builderListWrap = MkHBuilder . modifyOut (:[]) . HBuilderList

instance HBuilderCl HBuilderList where
    modifyIn  f (HBuilderList l) = HBuilderList $ map (modifyIn f) l
    modifyOut g (HBuilderList l) = HBuilderList $ map (modifyOut g) l
    runBuilder (HBuilderList l)  = accumList $ map runBuilder l

----------------------------------------------------------------

-- | Generic histogram builder. 
data HistBuilder st a b where
    HistBuilder :: Storage st => 
                   (a -> Input st) 
                -> (Output st -> b) 
                -> (forall s . ST s (st s))
                -> HistBuilder st a b

instance HBuilderCl (HistBuilder st) where
    modifyIn  f (HistBuilder inp out x) = HistBuilder (inp . f) out     x  
    modifyOut g (HistBuilder inp out x) = HistBuilder inp      (g .out) x  
    runBuilder  (HistBuilder inp out x) = do s <- x
                                             accumHist inp out s


----------------------------------------------------------------
-- Histogram constructors 
----------------------------------------------------------------

-- Convert from index to bin value
convert :: Bin bin => bin -> (a,[(Int,a)],a) -> (a,[(BinValue bin,a)],a)
convert bin (u,xs,o) = (u, map (first $ fromIndex bin) xs, o)

-- | Create histogram builder which take single item as input. Each item has weight 1.
mkHist1 :: (Bin bin) => bin                          -- ^ Bin information
        -> ( (Int, [(BinValue bin, Int)], Int) -> b) -- ^ Output function 
        -> (a -> BinValue bin)                       -- ^ Input function
        -> HBuilder a b                              
mkHist1 bin out inp = 
    let storage = newStorageUOne (getRange bin) (0 :: Int)
    in  MkHBuilder $ HistBuilder (toIndex bin . inp) (out . convert bin) storage

-- | Create histogram builder which take many items as input. Each item has weight 1.
mkHist :: (Bin bin) => bin                          -- ^ Bin information
       -> ( (Int, [(BinValue bin, Int)], Int) -> b) -- ^ Output function
       -> (a -> [BinValue bin])                     -- ^ Input function 
       -> HBuilder a b
mkHist bin out inp = 
    let storage = newStorageUMany (getRange bin) (0 :: Int)
    in  MkHBuilder $ HistBuilder (map (toIndex bin) . inp) (out . convert bin) storage

-- | Create histogram with weighted bin. Takes one item at time. 
mkHistWgh1 :: (Bin bin) => bin                                  -- ^ Bin information
          -> ( (Double, [(BinValue bin, Double)], Double) -> b) -- ^ Output function
          -> (a -> (BinValue bin, Double))                      -- ^ Input function
          -> HBuilder a b
mkHistWgh1 bin out inp = 
    let storage = newStorageUOneW (getRange bin) (0 :: Double)
    in  MkHBuilder $ HistBuilder (first (toIndex bin) . inp) (out . convert bin) storage

-- | Create histogram with weighted bin. Takes many items at time.
mkHistWgh :: (Bin bin) => bin                                   -- ^ Bin information
          -> ( (Double, [(BinValue bin, Double)], Double) -> b) -- ^ Output function
          -> (a -> [(BinValue bin, Double)])                    -- ^ Input function
          -> HBuilder a b
mkHistWgh bin out inp = 
    let storage = newStorageUManyW (getRange bin) (0 :: Double)
    in  MkHBuilder $ HistBuilder ((map $ first $ toIndex bin) . inp) (out . convert bin) storage

-- | Create histogram with list attached to each bin 
mkHistList1 :: Bin bin => bin               -- ^ Bin information
            -> ([(BinValue bin, [t])] -> b) -- ^ Output function
            -> (a -> (BinValue bin, t))     -- ^ Input function
            -> HBuilder a b
mkHistList1 bin out inp =
    let storage = newGenericStorage (:) ([]) (getRange bin) 
    in  MkHBuilder $ HistBuilder (first (toIndex bin) . inp) (out . map (first $ fromIndex bin)) storage

-- | Create histogram with list attached to each bin 
mkHistList :: Bin bin => bin                -- ^ Bin information
            -> ([(BinValue bin, [t])] -> b) -- ^ Output function
            -> (a -> [(BinValue bin, t)])   -- ^ Input function 
            -> HBuilder a b
mkHistList bin out inp =
    let storage = newGenericStorageMany (:) ([]) (getRange bin) 
    in  MkHBuilder $ HistBuilder ((map $ first $ toIndex bin) . inp) (out . map (first $ fromIndex bin)) storage
