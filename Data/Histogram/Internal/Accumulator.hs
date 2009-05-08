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
                                           , accumHistWgh

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
newtype AccumList s a b = AccumList [Accum s a b]
 
-- | Wrap list of histograms into one 'Accum'
accumList :: [ST s (Accum s a b)] -> ST s (Accum s a b)
accumList l = return . MkAccum . AccumList =<< sequence l

instance Accumulator AccumList where
    putOne (AccumList l) x = mapM_ (flip putOne $ x) l 
    extract (AccumList l)  = mconcat `fmap` mapM extract l 


----------------------------------------

{- Too general histogram.

 * inp - input type. Either `ix' or '(ix,v)'
 * ix  - index of array. 
 * v   - values of bin
 * s   - state variable 
 * a   - input type
 * b   - output type
-}
data AccumHistGen inp ix v s a b = AccumHistGen { histStIn      :: a -> [inp]
                                                , histStOut     :: (v, [(ix,v)], v) -> b
                                                , histStStorage :: Storage s ix v
                                                }


----------------------------------------

-- Histogram with fixed bin weight (1)
newtype AccumHist ix v s a b = AccumHist (AccumHistGen ix ix v s a b)

-- | Create accumulator histogram
accumHist :: (MArray (STUArray s) v (ST s), Ix ix, Num v, Accumulator (AccumHist ix v)) 
          => (a -> [ix]) 
          -> ((v, [(ix,v)], v) -> b)
          -> (ix, ix) 
          -> HistogramST s a b
accumHist fi fo rng = do st <- newStorage rng
                         return . MkAccum . AccumHist $ AccumHistGen fi fo st

histPut :: (Num v, Ix ix, MArray (STUArray s) v (ST s)) => AccumHist ix v s a b -> a -> ST s ()
histPut (AccumHist h) x = fillMany (histStStorage h) (histStIn h $ x)
{-# SPECIALIZE histPut :: AccumHist Int Int s a b          -> a -> ST s () #-}
{-# SPECIALIZE histPut :: AccumHist Int Double s a b       -> a -> ST s () #-}
{-# SPECIALIZE histPut :: AccumHist (Int,Int) Int s a b    -> a -> ST s () #-}
{-# SPECIALIZE histPut :: AccumHist (Int,Int) Double s a b -> a -> ST s () #-}

histExtract :: (Num v, Ix ix, MArray (STUArray s) v (ST s)) => AccumHist ix v s a b -> ST s b
histExtract (AccumHist h) = freezeStorage (histStStorage h) >>= return . (histStOut h)

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


-------------------------------------------------

-- Histogram with variable bin weight
newtype AccumHistWgh ix v s a b = AccumHistWgh (AccumHistGen (ix,v) ix v s a b)

-- | Create accumulator histogram with weighted bins
accumHistWgh :: (MArray (STUArray s) v (ST s), Ix ix, Num v, Accumulator (AccumHistWgh ix v)) 
          => (a -> [(ix,v)]) 
          -> ((v, [(ix,v)], v) -> b)
          -> (ix, ix) 
          -> HistogramST s a b
accumHistWgh fi fo rng = do st <- newStorage rng
                            return . MkAccum . AccumHistWgh $ AccumHistGen fi fo st

histPutWgh :: (Num v, Ix ix, MArray (STUArray s) v (ST s)) => AccumHistWgh ix v s a b -> a -> ST s ()
histPutWgh (AccumHistWgh h) x = fillManyWgh (histStStorage h) (histStIn h $ x)
{-# SPECIALIZE histPutWgh :: AccumHistWgh Int Int s a b          -> a -> ST s () #-}
{-# SPECIALIZE histPutWgh :: AccumHistWgh Int Double s a b       -> a -> ST s () #-}
{-# SPECIALIZE histPutWgh :: AccumHistWgh (Int,Int) Int s a b    -> a -> ST s () #-}
{-# SPECIALIZE histPutWgh :: AccumHistWgh (Int,Int) Double s a b -> a -> ST s () #-}

histExtractWgh :: (Num v, Ix ix, MArray (STUArray s) v (ST s)) => AccumHistWgh ix v s a b -> ST s b
histExtractWgh (AccumHistWgh h) = freezeStorage (histStStorage h) >>= return . (histStOut h)

instance Accumulator (AccumHistWgh Int Int) where 
    putOne  = histPutWgh 
    extract = histExtractWgh 
instance Accumulator (AccumHistWgh Int Double) where 
    putOne  = histPutWgh 
    extract = histExtractWgh 
instance Accumulator (AccumHistWgh (Int,Int) Int) where 
    putOne  = histPutWgh 
    extract = histExtractWgh 
instance Accumulator (AccumHistWgh (Int,Int) Double) where 
    putOne  = histPutWgh 
    extract = histExtractWgh 
