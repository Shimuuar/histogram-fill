{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE TypeFamilies     #-}
{-# LANGUAGE BangPatterns     #-}
-- |
-- Module     : Data.Histogram.Storage
-- Copyright  : Copyright (c) 2009, Alexey Khudyakov <alexey.skladnoy@gmail.com>
-- License    : BSD3
-- Maintainer : Alexey Khudyakov <alexey.skladnoy@gmail.com>
-- Stability  : experimental
-- 
-- Mutable storage for filling histograms. 
--
-- All storages return lists instead of array because array subscripts
-- has little meaning. They should be converted into bin values which
-- can be of any type and not suitable as array index.

module Data.Histogram.Internal.Storage ( Storage(..)
                                       , newStorageUOne
                                       , newStorageUMany
                                       , newStorageUOneW
                                       , newStorageUManyW
                                       , newGenericStorage
                                       , newGenericStorageMany
                                       -- * Storage types
                                       , StorageUOne
                                       , StorageUMany
                                       , StorageUOneW
                                       , StorageUManyW
                                       , GenericStorage
                                       , GenericStorageMany
                                       ) where

import Control.Monad (forM_)
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
newtype StorageUOne v s = StorageUOne (StorageUbox s v)
-- | Create new storage 
newStorageUOne :: (Num v, MArray (STUArray s) v (ST s)) => (Int,Int) -> v -> ST s (StorageUOne v s) 
newStorageUOne r x = fmap StorageUOne $ newStorageUbox r x

instance Storage (StorageUOne v) where
    type Input   (StorageUOne v) = Int
    type Output  (StorageUOne v) = (v,[(Int,v)],v)
    putItem       (StorageUOne st) i = fillOne st i
    freezeStorage (StorageUOne st)   = freezeStorageUbox st


-- | Unboxed storage for numeric histograms.
newtype StorageUMany v s = StorageUMany (StorageUbox s v)
-- | Create new storage 
newStorageUMany :: (Num v, MArray (STUArray s) v (ST s)) => (Int,Int) -> v -> ST s (StorageUMany v s) 
newStorageUMany r x = fmap StorageUMany $ newStorageUbox r x

instance Storage (StorageUMany v) where
    type Input   (StorageUMany v) = [Int]
    type Output  (StorageUMany v) = (v,[(Int,v)],v)
    putItem       (StorageUMany st) i = fillMany st i
    freezeStorage (StorageUMany st)   = freezeStorageUbox st


-- | Unboxed storage for numeric histograms.
newtype StorageUOneW v s = StorageUOneW (StorageUbox s v)
-- | Create new storage 
newStorageUOneW :: (Num v, MArray (STUArray s) v (ST s)) => (Int,Int) -> v -> ST s (StorageUOneW v s) 
newStorageUOneW r x = fmap StorageUOneW $ newStorageUbox r x

instance Storage (StorageUOneW v) where
    type Input   (StorageUOneW v) = (Int,v)
    type Output  (StorageUOneW v) = (v,[(Int,v)],v)
    putItem       (StorageUOneW st) i = fillOneWgh st i
    freezeStorage (StorageUOneW st)   = freezeStorageUbox st


-- | Unboxed storage for numeric histograms.
newtype StorageUManyW v s = StorageUManyW (StorageUbox s v)
-- | Create new storage 
newStorageUManyW :: (Num v, MArray (STUArray s) v (ST s)) => (Int,Int) -> v -> ST s (StorageUManyW v s) 
newStorageUManyW r x = fmap StorageUManyW $ newStorageUbox r x

instance Storage (StorageUManyW v) where
    type Input   (StorageUManyW v) = [(Int,v)]
    type Output  (StorageUManyW v) = (v,[(Int,v)],v)
    putItem       (StorageUManyW st) i = fillManyWgh st i
    freezeStorage (StorageUManyW st)   = freezeStorageUbox st



----------------------------------------------------------------
-- Implementation 
----------------------------------------------------------------

-- | Mutable storage for histograms. 
data StorageUbox s v where
    StorageUbox :: (Num v, MArray (STUArray s) v (ST s)) 
            => STRef s v
            -> STUArray s Int v 
            -> STRef s v 
            -> StorageUbox s v

-- | Create empty mutable storage. Everything is filled with zeroes
newStorageUbox :: (Num v, MArray (STUArray s) v (ST s)) => (Int,Int) -> v -> ST s (StorageUbox s v) 
newStorageUbox r x = do arr <- newArray r x
                        u   <- newSTRef x
                        o   <- newSTRef x
                        return $ StorageUbox u arr o


-- | Put value into storage with checking for under/overflows. It is
--   specialized for Int and Double for perfomance. 
fillOne :: StorageUbox s v -> Int -> ST s ()
fillOne !(StorageUbox u hist o) !i = do
  (lo,hi) <- getBounds hist
  if i < lo then modifySTRef u (+1)
            else if i > hi then modifySTRef o ((+1) $! )
                           else (writeArray hist i . (+1) =<< readArray hist i)
{-# SPECIALIZE fillOne :: StorageUbox s Int    -> Int -> ST s () #-}
{-# SPECIALIZE fillOne :: StorageUbox s Double -> Int -> ST s () #-}

-- | Put value into histogram with weight.
fillOneWgh :: StorageUbox s v -> (Int,v) -> ST s ()
fillOneWgh !(StorageUbox u hist o) !(i,w) = do
  (lo,hi) <- getBounds hist
  if i < lo then modifySTRef u ((+w) $!)
            else if i > hi then modifySTRef o (+w)
                           else (writeArray hist i . (+w) =<< readArray hist i)
{-# SPECIALIZE fillOneWgh :: StorageUbox s Int    -> (Int,Int)    -> ST s () #-}
{-# SPECIALIZE fillOneWgh :: StorageUbox s Double -> (Int,Double) -> ST s () #-}

-- | Put list of values into storage
fillMany :: StorageUbox s v -> [Int] -> ST s ()
fillMany h = mapM_ (fillOne h)
-- FIXME: check effect of INLINE pragma on perfomance
{-# INLINE fillMany #-}

-- | Put list of values with weight into storage
fillManyWgh :: StorageUbox s v -> [(Int,v)] -> ST s ()
fillManyWgh h = mapM_ (fillOneWgh h)

-- | Convert storage to immutable form.
freezeStorageUbox :: StorageUbox s v -> ST s (v, [(Int,v)], v)
freezeStorageUbox (StorageUbox uST histST oST) = do
  u <- readSTRef uST
  o <- readSTRef oST
  a <- getAssocs histST
  return (u,a,o)


----------------------------------------------------------------
-- Generic storage
----------------------------------------------------------------

data GenericStorage x v s where
    GenericStorage :: (x -> v -> v) -> (STArray s Int v) -> GenericStorage x v s

newGenericStorage :: (x -> v -> v) -> v -> (Int,Int) -> ST s (GenericStorage x v s)
newGenericStorage cmb def rng = do arr <- newArray rng def 
                                   return $ GenericStorage cmb arr

instance Storage (GenericStorage x v) where
    type Input (GenericStorage x v) = (Int,x)
    type Output (GenericStorage x v) = [(Int,v)]
    putItem (GenericStorage f arr) (i,x) = readArray arr i >>= writeArray arr i . f x
    freezeStorage (GenericStorage _ arr) = getAssocs arr


newtype GenericStorageMany x v s = GenericStorageMany (GenericStorage x v s)

newGenericStorageMany :: (x -> v -> v) -> v -> (Int,Int) -> ST s (GenericStorageMany x v s)
newGenericStorageMany cmb def rng = do arr <- newArray rng def 
                                       return $ GenericStorageMany (GenericStorage cmb arr)

instance Storage (GenericStorageMany x v) where
    type Input   (GenericStorageMany x v) = [(Int,x)]
    type Output  (GenericStorageMany x v) = [(Int,v)]
    putItem (GenericStorageMany (GenericStorage f arr)) xs = 
        forM_ xs (\(i,x) -> readArray arr i >>= writeArray arr i . f x)
    freezeStorage (GenericStorageMany (GenericStorage _ arr)) =
        getAssocs arr
