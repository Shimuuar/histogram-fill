{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE GADTs #-}
-- |
-- Module     : Text.Flat
-- Copyright  : Copyright (c) 2009, Alexey Khudyakov <alexey.skladnoy@gmail.com>
-- License    : BSD3
-- Maintainer : Alexey Khudyakov <alexey.skladnoy@gmail.com>
-- Stability  : experimental
-- 
module Data.Histogram.Internal.Accumulator ( Accumulator(..)
                                           , Accum(MkAccum)
                                           , HistogramST 

                                           , accumList
                                           , accumHist

                                           , fillHistograms
                                           ) where

import Control.Monad
import Control.Monad.ST

import Data.Array
import Data.Array.ST
import Data.Ix
import Data.Monoid

import Data.Histogram.Internal.Storage


----------------------------------------------------------------

-- | Put all values into histogram and return result
fillHistograms :: Monoid b => (forall s . ST s (Accum s a b)) -> [a] -> b
fillHistograms h xs = runST $ do h' <- h
                                 putMany h' xs
                                 extract h'


----------------------------------------------------------------
-- Accumulator typeclass
----------------------------------------------------------------
-- | Typeclass which reperesent fillable histogram.
class Accumulator h where
    -- | Put one element into accumulator
    putOne  :: h s a b -> a   -> ST s () 
    -- | Put many elements at once 
    putMany :: h s a b -> [a] -> ST s () 
    putMany h = mapM_ (putOne h) 
    -- | Extract data from historam 
    extract :: Monoid b => (h s a b) -> ST s b
    


----------------------------------------------------------------
-- GADT wrapper 
----------------------------------------------------------------
-- | GATDed wrapper for histogram builders
data Accum s a b where
    MkAccum :: Accumulator h => h s a b -> Accum s a b

-- | Synonym for @ST s (Accum s a)@
type HistogramST s a b = ST s (Accum s a b)

instance Accumulator Accum where
    putOne  (MkAccum h) x = putOne h x 
    extract (MkAccum h)   = extract h



----------------------------------------------------------------
-- List of histograms
----------------------------------------------------------------
newtype AccumList s a b = AccumList [Accum s a b]
 
-- | Wrap list of histograms into one 'Accum'
accumList :: [ST s (Accum s a b)] -> ST s (Accum s a b)
accumList l = return . MkAccum . AccumList =<< sequence l

instance Accumulator AccumList where
    putOne (AccumList l) x = mapM_ (flip putOne $ x) l 
    extract (AccumList l)  = mconcat `fmap` mapM extract l 



----------------------------------------------------------------
-- Generic histogram 
----------------------------------------------------------------
data AccumHist st s a b where
    AccumHist :: Storage st => (a -> Input st) -> (Output st -> b) -> st s -> AccumHist st s a b

accumHist :: Storage st => (a -> Input st) -> (Output st -> b) -> st s -> HistogramST s a b
accumHist inp out st = return . MkAccum $ AccumHist inp out st

instance Accumulator (AccumHist st) where
    putOne  (AccumHist inp _ st) x = putItem st (inp x)
    extract (AccumHist _ out st)   = out `fmap` freezeStorage st
