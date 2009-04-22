{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types #-}
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

import Data.Array.ST
import Data.Ix
import Data.Monoid

import Data.Histogram.Internal.Storage


----------------------------------------------------------------

-- | Convinience function for multiple histogram creation
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
-- Existential wrapper 
----------------------------------------------------------------

-- | Existestial wrapper for all types of histogram
data Accum s a b = forall h . Accumulator h => MkAccum (h s a b)

-- | Synonym for @ST s (Accum s a)@
type HistogramST s a b = ST s (Accum s a b)

instance Accumulator Accum where
    putOne  (MkAccum h) x = putOne h x 
    extract (MkAccum h)   = extract h

----------------------------------------------------------------
-- Concrete histograms
----------------------------------------------------------------
-- List of histograms
data AccumList s a b = AccumList [Accum s a b]
 
-- | Wrap list of histograms into one 'Accum'
accumList :: [ST s (Accum s a b)] -> ST s (Accum s a b)
accumList l = return . MkAccum . AccumList =<< sequence l

instance Accumulator AccumList where
    putOne (AccumList l) x = mapM_ (flip putOne $ x) l 
    extract (AccumList l)  = liftM mconcat $ mapM extract l 



data AccumHist ix v s a b = AccumHist { histStIn      :: a -> [ix]
                                      , histStOut     :: (v, [(ix,v)], v) -> b
                                      , histStStorage :: Storage s ix v
                                      }

-- | Create accumulator histogram
accumHist :: (MArray (STUArray s) v (ST s), Ix ix, Num v, Accumulator (AccumHist ix v)) 
          => (a -> [ix]) 
          -> ((v, [(ix,v)], v) -> b)
          -> (ix, ix) 
          -> HistogramST s a b
accumHist fi fo rng = do st <- newStorage rng
                         return . MkAccum $ AccumHist fi fo st

histPut :: (Num v, Ix ix, MArray (STUArray s) v (ST s)) => AccumHist ix v s a b -> a -> ST s ()
histPut h x = fillMany (histStStorage h) (histStIn h $ x)

histExtract :: (Num v, Ix ix, MArray (STUArray s) v (ST s)) => AccumHist ix v s a b -> ST s b
histExtract h = freezeStorage (histStStorage h) >>= return . (histStOut h)

instance Accumulator (AccumHist Int Int) where 
    putOne  = histPut
    extract = histExtract 
instance Accumulator (AccumHist Int Double) where 
    putOne  = histPut
    extract = histExtract 
instance Accumulator (AccumHist (Int,Int) Int) where 
    putOne  = histPut
    extract = histExtract 
instance Accumulator (AccumHist (Int,Int) Double) where 
    putOne  = histPut
    extract = histExtract
