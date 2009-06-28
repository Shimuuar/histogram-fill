{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE TypeFamilies     #-}
-- |
-- Module     : Text.Flat
-- Copyright  : Copyright (c) 2009, Alexey Khudyakov <alexey.skladnoy@gmail.com>
-- License    : BSD3
-- Maintainer : Alexey Khudyakov <alexey.skladnoy@gmail.com>
-- Stability  : experimental
-- 
-- Mutable storage for filling histograms. 

module Data.Histogram.Internal.Storage ( Storage(..)
                                       , newStorageUOne
                                       , newStorageUMany
                                       , newStorageUOneW
                                       , newStorageUManyW
                                       , newGenericStorage

                                       , StorageUOne
                                       , StorageUMany
                                       , StorageUOneW
                                       , StorageUManyW
                                       , GenericStorage
                                       ) where

import Control.Monad.ST

import Data.Array.ST
import Data.STRef

----------------------------------------------------------------

-- | Type class which represent mutable storage
class Storage a where
    -- | Type of item which are put into storage 
    type Input  a 
    -- | Type of retrieved from storage 
    type Output a
    -- | Put one item into storage
    putItem :: a s -> Input a -> ST s ()
    -- | Retreive stroage
    freezeStorage :: a s -> ST s (Output a)


----------------------------------------------------------------

-- | Unboxed storage for numeric histograms.
newtype StorageUOne i v s = StorageUOne (StorageUbox s i v)
-- | Create new storage 
newStorageUOne :: (Num v, Ix i, MArray (STUArray s) v (ST s)) => (i,i) -> ST s (StorageUOne i v s) 
newStorageUOne = fmap StorageUOne . newStorageUbox 

instance Storage (StorageUOne i v) where
    type Input   (StorageUOne i v) = i
    type Output  (StorageUOne i v) = (v,[(i,v)],v)
    putItem       (StorageUOne st) i = fillOne st i
    freezeStorage (StorageUOne st)   = freezeStorageUbox st


-- | Unboxed storage for numeric histograms.
newtype StorageUMany i v s = StorageUMany (StorageUbox s i v)
-- | Create new storage 
newStorageUMany :: (Num v, Ix i, MArray (STUArray s) v (ST s)) => (i,i) -> ST s (StorageUMany i v s) 
newStorageUMany = fmap StorageUMany . newStorageUbox 

instance Storage (StorageUMany i v) where
    type Input   (StorageUMany i v) = [i]
    type Output  (StorageUMany i v) = (v,[(i,v)],v)
    putItem       (StorageUMany st) i = fillMany st i
    freezeStorage (StorageUMany st)   = freezeStorageUbox st


-- | Unboxed storage for numeric histograms.
newtype StorageUOneW i v s = StorageUOneW (StorageUbox s i v)
-- | Create new storage 
newStorageUOneW :: (Num v, Ix i, MArray (STUArray s) v (ST s)) => (i,i) -> ST s (StorageUOneW i v s) 
newStorageUOneW = fmap StorageUOneW . newStorageUbox 

instance Storage (StorageUOneW i v) where
    type Input   (StorageUOneW i v) = (i,v)
    type Output  (StorageUOneW i v) = (v,[(i,v)],v)
    putItem       (StorageUOneW st) i = fillOneWgh st i
    freezeStorage (StorageUOneW st)   = freezeStorageUbox st


-- | Unboxed storage for numeric histograms.
newtype StorageUManyW i v s = StorageUManyW (StorageUbox s i v)
-- | Create new storage 
newStorageUManyW :: (Num v, Ix i, MArray (STUArray s) v (ST s)) => (i,i) -> ST s (StorageUManyW i v s) 
newStorageUManyW = fmap StorageUManyW . newStorageUbox 

instance Storage (StorageUManyW i v) where
    type Input   (StorageUManyW i v) = [(i,v)]
    type Output  (StorageUManyW i v) = (v,[(i,v)],v)
    putItem       (StorageUManyW st) i = fillManyWgh st i
    freezeStorage (StorageUManyW st)   = freezeStorageUbox st



----------------------------------------------------------------
-- Implementation 
----------------------------------------------------------------

-- | Mutable storage for histograms. 
data StorageUbox s i v where
    StorageUbox :: (Num v, Ix i, MArray (STUArray s) v (ST s)) 
            => STRef s v
            -> STUArray s i v 
            -> STRef s v 
            -> StorageUbox s i v

-- | Create empty mutable storage. Everything is filled with zeroes
newStorageUbox :: (Num v, Ix i, MArray (STUArray s) v (ST s)) => (i,i) -> ST s (StorageUbox s i v) 
newStorageUbox r = do arr <- newArray r 0
                      u   <- newSTRef 0
                      o   <- newSTRef 0
                      return $ StorageUbox u arr o


-- | Put value into storage with checking for under/overflows. It is
--   specialized for Int and Double for perfomance. 
fillOne :: StorageUbox s i v -> i -> ST s ()
fillOne (StorageUbox u hist o) i = do
  (lo,hi) <- getBounds hist
  if i < lo then modifySTRef u (+1)
            else if i > hi then modifySTRef o ((+1) $! )
                           else (writeArray hist i . (+1) =<< readArray hist i)
{-# SPECIALIZE fillOne :: StorageUbox s Int Int    -> Int -> ST s () #-}
{-# SPECIALIZE fillOne :: StorageUbox s Int Double -> Int -> ST s () #-}
{-# SPECIALIZE fillOne :: StorageUbox s (Int,Int) Int    -> (Int,Int) -> ST s () #-}
{-# SPECIALIZE fillOne :: StorageUbox s (Int,Int) Double -> (Int,Int) -> ST s () #-}

{-| Put value into histogram with weight.
 -}
fillOneWgh :: StorageUbox s i v -> (i,v) -> ST s ()
fillOneWgh (StorageUbox u hist o) (i,w) = do
  (lo,hi) <- getBounds hist
  if i < lo then modifySTRef u ((+w) $!)
            else if i > hi then modifySTRef o (+w)
                           else (writeArray hist i . (+w) =<< readArray hist i)
{-# SPECIALIZE fillOneWgh :: StorageUbox s Int Int    -> (Int,Int)    -> ST s () #-}
{-# SPECIALIZE fillOneWgh :: StorageUbox s Int Double -> (Int,Double) -> ST s () #-}
{-# SPECIALIZE fillOneWgh :: StorageUbox s (Int,Int) Int    -> ((Int,Int),Int)    -> ST s () #-}
{-# SPECIALIZE fillOneWgh :: StorageUbox s (Int,Int) Double -> ((Int,Int),Double) -> ST s () #-}

-- | Put list of values into storage
fillMany :: StorageUbox s i v -> [i] -> ST s ()
fillMany h = mapM_ (fillOne h)
-- FIXME: check effect of INLINE pragma on perfomance
{-# INLINE fillMany #-}

-- | Put list of values with weight into storage
fillManyWgh :: StorageUbox s i v -> [(i,v)] -> ST s ()
fillManyWgh h = mapM_ (fillOneWgh h)

-- | Convert storage to immutable form.
freezeStorageUbox :: StorageUbox s i v -> ST s (v, [(i,v)], v)
freezeStorageUbox (StorageUbox uST histST oST) = do
  u <- readSTRef uST
  o <- readSTRef oST
  a <- getAssocs histST
  return (u,a,o)


----------------------------------------------------------------
-- Generic storage
----------------------------------------------------------------

data GenericStorage x i v s where
    GenericStorage :: Ix i => (x -> v -> v) -> (STArray s i v) -> GenericStorage x i v s

newGenericStorage :: Ix i => (x -> v -> v) -> v -> (i,i) -> ST s (GenericStorage x i v s)
newGenericStorage cmb def rng = do arr <- newArray rng def 
                                   return $ GenericStorage cmb arr

instance Storage (GenericStorage x i v) where
    type Input (GenericStorage x i v) = (i,x)
    type Output (GenericStorage x i v) = [(i,v)]
    putItem (GenericStorage f arr) (i,x) = do v <- readArray arr i
                                              writeArray arr i (f x v)
    freezeStorage (GenericStorage _ arr) = getAssocs arr
