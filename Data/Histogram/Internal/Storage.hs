{-# LANGUAGE FlexibleContexts #-}
-- |
-- Module     : Text.Flat
-- Copyright  : Copyright (c) 2009, Alexey Khudyakov <alexey.skladnoy@gmail.com>
-- License    : BSD3
-- Maintainer : Alexey Khudyakov <alexey.skladnoy@gmail.com>
-- Stability  : experimental
-- 
-- Mutable storage for filling histograms. 

module Data.Histogram.Internal.Storage ( Storage(..)
                                       , newStorage
                                       , fillOne
                                       , fillOneWgh
                                       , fillMany
                                       , fillManyWgh
                                       , freezeStorage
                                       ) where 

import Control.Monad.ST

import Data.Array.ST
import Data.STRef

-- | Mutable storage for histograms. 
data Storage s i v = Storage { underflowsST :: STRef s v
                             , histDataST   :: STUArray s i v
                             , overflowsST  :: STRef s v
                             }


-- | Create empty mutable storage. Everything is filled with zeroes
newStorage :: (Num v, Ix i, MArray (STUArray s) v (ST s)) => (i,i) -> ST s (Storage s i v) 
newStorage r = do arr <- newArray r 0
                  u   <- newSTRef 0
                  o   <- newSTRef 0
                  return $ Storage u arr o


{-| Put value into storage with checking for under/overflows. It is
  specialized for Int and Double for perfomance. 
  
  FIXME: there is strictness problem with STRefs. If there is too
  many modifcations it cause stack overflow on evaluating
  -}
fillOne :: (Num v, Ix i, MArray (STUArray s) v (ST s)) => Storage s i v -> i -> ST s ()
fillOne (Storage u hist o) i = do
  (lo,hi) <- getBounds hist
  if i < lo then modifySTRef u (+1)
            else if i > hi then modifySTRef o (+1)
                           else (writeArray hist i . (+1) =<< readArray hist i)
{-# SPECIALIZE fillOne :: Storage s Int Int    -> Int -> ST s () #-}
{-# SPECIALIZE fillOne :: Storage s Int Double -> Int -> ST s () #-}
{-# SPECIALIZE fillOne :: Storage s (Int,Int) Int    -> (Int,Int) -> ST s () #-}
{-# SPECIALIZE fillOne :: Storage s (Int,Int) Double -> (Int,Int) -> ST s () #-}

{-| Put value into histogram with weight.
 -}
fillOneWgh :: (Num v, Ix i, MArray (STUArray s) v (ST s)) => Storage s i v -> (i,v) -> ST s ()
fillOneWgh (Storage u hist o) (i,w) = do
  (lo,hi) <- getBounds hist
  if i < lo then modifySTRef u (+w)
            else if i > hi then modifySTRef o (+w)
                           else (writeArray hist i . (+w) =<< readArray hist i)
{-# SPECIALIZE fillOneWgh :: Storage s Int Int    -> (Int,Int)    -> ST s () #-}
{-# SPECIALIZE fillOneWgh :: Storage s Int Double -> (Int,Double) -> ST s () #-}
{-# SPECIALIZE fillOneWgh :: Storage s (Int,Int) Int    -> ((Int,Int),Int)    -> ST s () #-}
{-# SPECIALIZE fillOneWgh :: Storage s (Int,Int) Double -> ((Int,Int),Double) -> ST s () #-}

-- | Put list of values into storage
fillMany :: (Num v, Ix i, MArray (STUArray s) v (ST s)) => Storage s i v -> [i] -> ST s ()
fillMany h = mapM_ (fillOne h)
-- FIXME: check effect of INLINE pragma on perfomance
{-# INLINE fillMany #-}

-- | Put list of values with weight into storage
fillManyWgh :: (Num v, Ix i, MArray (STUArray s) v (ST s)) => Storage s i v -> [(i,v)] -> ST s ()
fillManyWgh h = mapM_ (fillOneWgh h)

-- | Convert storage to immutable form.
freezeStorage :: (Ix i, MArray (STUArray s) v (ST s)) => Storage s i v -> ST s (v, [(i,v)], v)
freezeStorage st = do u <- readSTRef $ underflowsST st
                      o <- readSTRef $ overflowsST st
                      a <- getAssocs $ histDataST st
                      return (u,a,o)
