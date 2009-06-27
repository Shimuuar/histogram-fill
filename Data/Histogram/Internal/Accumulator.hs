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
                                           , accumHistWgh
                                           , accumGeneric

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
-- Existential wrapper 
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
-- Numeric histograms 
----------------------------------------------------------------

-- Too general histogram.
--
-- * inp - input type. Either `ix' or '(ix,v)'
-- * ix  - index of array. 
-- * v   - values of bin
-- * s   - state variable 
-- * a   - input type
-- * b   - output type
data AccumHistGen inp ix v s a b = AccumHistGen { histStIn      :: a -> [inp]
                                                , histStOut     :: (v, [(ix,v)], v) -> b
                                                , histStStorage :: Storage s ix v
                                                }

----------------------------------------------------------------
-- Histogram with fixed bin weight (1)
newtype AccumHist ix v s a b = AccumHist (AccumHistGen ix ix v s a b)

-- | Create accumulator histogram
accumHist :: (MArray (STUArray s) v (ST s), Ix ix, Num v)
          => (a -> [ix]) 
          -> ((v, [(ix,v)], v) -> b)
          -> (ix, ix) 
          -> HistogramST s a b
accumHist fi fo rng = do st <- newStorage rng
                         return . MkAccum . AccumHist $ AccumHistGen fi fo st

instance Accumulator (AccumHist i v) where 
    putOne  (AccumHist h) x = fillMany (histStStorage h) (histStIn h $ x)
    {-# INLINE putOne #-}
    extract (AccumHist h)   = freezeStorage (histStStorage h) >>= return . (histStOut h)


----------------------------------------------------------------
-- Histogram with variable bin weight
newtype AccumHistWgh ix v s a b = AccumHistWgh (AccumHistGen (ix,v) ix v s a b)

-- | Create accumulator histogram with weighted bins
accumHistWgh :: (MArray (STUArray s) v (ST s), Ix ix, Num v)
          => (a -> [(ix,v)]) 
          -> ((v, [(ix,v)], v) -> b)
          -> (ix, ix) 
          -> HistogramST s a b
accumHistWgh fi fo rng = do st <- newStorage rng
                            return . MkAccum . AccumHistWgh $ AccumHistGen fi fo st

instance Accumulator (AccumHistWgh i v) where 
    putOne  (AccumHistWgh h) x = fillManyWgh (histStStorage h) (histStIn h $ x)
    {-# INLINE putOne #-}
    extract (AccumHistWgh h)   = freezeStorage (histStStorage h) >>= return . (histStOut h)


----------------------------------------------------------------
-- Histogram with generic bin contents
data AccumGeneric i v s a b = AccumGeneric { genericStIn      :: a -> [(i,v)]
                                           , genericStOut     :: [(i,v)] -> b
                                           , genericStCombine   :: v -> v -> v
                                           , genericStStorage :: STArray s i v
                                           }


accumGeneric :: Ix i => (a -> [(i,v)]) -> ([(i,v)] -> b) -> (v -> v -> v) -> (i,i) -> v -> HistogramST s a b
accumGeneric inp out cmb r def = do st <- newArray r def
                                    return . MkAccum $ AccumGeneric inp out cmb st

instance Ix i => Accumulator (AccumGeneric i v) where
    putOne h x = do
      forM_ (genericStIn h $ x) $ \(i,v) -> do
        let arr = genericStStorage h
            cmb = genericStCombine h
        rng <- getBounds arr
        when (inRange rng i) $ readArray arr i >>= writeArray arr i . cmb v
    extract h  = (genericStOut h . assocs) `fmap` freeze (genericStStorage h)
