{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE Rank2Types #-}
module Data.Histogram.Fill ( -- * Typeclasses and existentials 
                             HBuilderCl(..)
                           , HBuilder
                           , builderList
                           , builderListWrap

                           -- * Fill routines
                           , createHistograms

                           -- * 1D histograms
                           , mkHist1Dint1
                           , mkHist1Dint
                           , mkHist1DintList
                           , mkHist1DintListMany
                           -- * 2D histograms
                           , mkHist2Dint1
                           , mkHist2Dint
                           , mkHist2DintList
                           , mkHist2DintListMany
                           -- * Internals 
                           , HistBuilder

                           ) where

import Control.Monad.ST (ST)
import Data.Monoid      (Monoid)

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

-- | 1D histogram with integer bins
mkHist1Dint1 :: ((Int,[(Int,Int)],Int) -> b) -- ^ Output function
            -> (Int, Int)                    -- ^ Histogram range 
            -> (a -> Int)                    -- ^ Input function
            -> HBuilder a b
mkHist1Dint1 out rng inp = 
    let storage = newStorageUOne rng :: ST s (StorageUOne Int Int s)
    in  MkHBuilder $ HistBuilder inp out storage

-- | 1D histogram with ineteger bins 
mkHist1Dint :: ((Int,[(Int,Int)],Int) -> b) -- ^ Output function
            -> (Int, Int)                   -- ^ Histogram range 
            -> (a -> [Int])                 -- ^ Input function
            -> HBuilder a b
mkHist1Dint out rng inp = 
    let storage = newStorageUMany rng :: ST s (StorageUMany Int Int s)
    in  MkHBuilder $ HistBuilder inp out storage

-- | 1D histogram with int bins and list contents
mkHist1DintList :: ([(Int,[v])] -> b)
                -> (Int, Int) 
                -> (a -> (Int,v)) 
                -> HBuilder a b
mkHist1DintList out rng inp = 
    let storage = newGenericStorage (:) [] rng :: ST s (GenericStorage v Int [v] s)
    in  MkHBuilder $ HistBuilder inp out storage
 
-- | 1D histogram with int bins and list contents
mkHist1DintListMany :: ([(Int,[v])] -> b)
                -> (Int, Int) 
                -> (a -> [(Int,v)]) 
                -> HBuilder a b
mkHist1DintListMany out rng inp = 
    let storage = newGenericStorageMany (:) [] rng :: ST s (GenericStorageMany v Int [v] s)
    in  MkHBuilder $ HistBuilder inp out storage
----------------

mk2Drange :: ((a,a), (a,a)) -> ((a,a), (a,a))
mk2Drange ((xmin,xmax), (ymin,ymax)) = ((xmin,ymin), (xmax, ymax))

-- | 2D historam with inetegr bins 
mkHist2Dint1 :: ((Int, [((Int,Int), Int)], Int) -> b) -- ^ Output function
             -> ((Int,Int), (Int,Int))                -- ^ Histogram range: (xmin,xmax), (ymin,ymax)
             -> (a -> (Int,Int))                      -- ^ Input function
             -> HBuilder a b
mkHist2Dint1 out rng inp = 
    let storage = newStorageUOne (mk2Drange rng) :: ST s (StorageUOne (Int,Int) Int s)
    in MkHBuilder $ HistBuilder inp out storage 


-- | 2D histogram with ineteger bins 
mkHist2Dint :: ((Int, [((Int,Int), Int)], Int) -> b) -- ^ Output function
            -> ((Int,Int), (Int,Int))                -- ^ Histogram range: (xmin,xmax), (ymin,ymax)
            -> (a -> [(Int,Int)])                    -- ^ Input function
            -> HBuilder a b
mkHist2Dint out rng inp = 
    let storage = newStorageUMany (mk2Drange rng) :: ST s (StorageUMany (Int,Int) Int s)
    in MkHBuilder $ HistBuilder inp out storage 

-- | 2D histogram with int bins and list contents
mkHist2DintList :: ([((Int,Int), [v])] -> b)
                -> ((Int,Int), (Int,Int)) 
                -> (a -> ((Int,Int),v)) 
                -> HBuilder a b
mkHist2DintList out rng inp = 
    let storage = newGenericStorage (:) [] (mk2Drange rng) :: ST s (GenericStorage v (Int,Int) [v] s)
    in  MkHBuilder $ HistBuilder inp out storage

-- | 2D histogram with int bins and list contents
mkHist2DintListMany :: ([((Int,Int), [v])] -> b)
                    -> ((Int,Int), (Int,Int)) 
                    -> (a -> [((Int,Int),v)]) 
                    -> HBuilder a b
mkHist2DintListMany out rng inp = 
    let storage = newGenericStorageMany (:) [] (mk2Drange rng) :: ST s (GenericStorageMany v (Int,Int) [v] s)
    in  MkHBuilder $ HistBuilder inp out storage
