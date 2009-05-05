{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.Histogram.Fill ( HBuilderCl(..)
                           , HBuilder
                           , HistBuilder
                           , builderList

                           , mkHistogram
                           , mkHist1Dint
                           , mkHist2Dint

                           , createHistograms
                           ) where

import Data.Monoid


import Data.Histogram.Internal.Accumulator
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

-- | Existential type. Histogram builder. 
data HBuilder a b = forall h . HBuilderCl h => MkHBuilder (h a b)

instance HBuilderCl HBuilder where 
    modifyIn  f (MkHBuilder h) = MkHBuilder $ modifyIn f h
    modifyOut g (MkHBuilder h) = MkHBuilder $ modifyOut g h 
    runBuilder (MkHBuilder h)  = runBuilder h


----------------------------------------------------------------

-- List of histograms. 
newtype HBuilderList a b = HBuilderList [HBuilder a b]

-- | Wrap list of histogram builders into HBuilder existential
builderList :: [HBuilder a b] -> HBuilder a b
builderList = MkHBuilder . HBuilderList

instance HBuilderCl HBuilderList where
    modifyIn  f (HBuilderList l) = HBuilderList $ map (modifyIn f) l
    modifyOut g (HBuilderList l) = HBuilderList $ map (modifyOut g) l
    runBuilder (HBuilderList l)  = accumList $ map runBuilder l

----------------------------------------------------------------

-- | Generic histogram builder. It's designed to be as general as possible. 
-- 
-- ix is supposed to be of Ix typeclass, v of Num.
data HistBuilder ix v a b = HistBuilder { histRange :: (ix,ix)   -- ^ Range of histogram
                                        , histIn    :: a -> [ix] -- ^Input function
                                        -- | Output: (underflows, [(nbin, value)], overflows) 
                                        , histOut   :: (v,[(ix, v)],v) -> b 
                                        }

modifyInF :: (a' -> a) -> HistBuilder ix v a b -> HistBuilder ix v a' b
modifyInF f h = h { histIn  = histIn h . f }

modifyOutF :: (b -> b') -> HistBuilder ix v a b -> HistBuilder ix v a b'
modifyOutF g h = h { histOut = g . histOut h }

runBuilderF h = accumHist (histIn h) (histOut h) (histRange h)

instance HBuilderCl (HistBuilder Int Int)  where 
    modifyIn  = modifyInF
    modifyOut = modifyOutF
    runBuilder = runBuilderF
instance HBuilderCl (HistBuilder Int Double)  where 
    modifyIn  = modifyInF
    modifyOut = modifyOutF
    runBuilder = runBuilderF
instance HBuilderCl (HistBuilder (Int,Int) Int)  where 
    modifyIn  = modifyInF
    modifyOut = modifyOutF
    runBuilder = runBuilderF
instance HBuilderCl (HistBuilder (Int,Int) Double)  where 
    modifyIn  = modifyInF
    modifyOut = modifyOutF
    runBuilder = runBuilderF

----------------------------------------------------------------

-- | Generic function to create histogram builder. 
mkHistogram :: (HBuilderCl (HistBuilder ix v)) =>
               ((v, [(ix, v)], v) -> b) -- ^ Output function 
            -> (ix, ix)                 -- ^ Histogram range
            -> (a -> [ix])              -- ^ Input function
            -> HBuilder a b
mkHistogram fout rng fin = MkHBuilder $ HistBuilder rng fin fout

-- | Create 1D histogram with integer bins. Just a type-specialized version of mkHistogram
mkHist1Dint :: (HBuilderCl (HistBuilder Int Int)) => 
               ((Int, [(Int, Int)], Int) -> b) 
            -> (Int, Int)
            -> (a -> [Int]) 
            -> HBuilder a b
mkHist1Dint = mkHistogram


-- | Create 2D histogram with intger bins. Just a type-specialized version of mkHistogram
mkHist2Dint :: (HBuilderCl (HistBuilder (Int,Int) Int)) => 
               ((Int, [((Int,Int), Int)], Int) -> b)
            -> ((Int,Int), (Int,Int)) 
            -> (a -> [(Int,Int)]) 
            -> HBuilder a b
mkHist2Dint = mkHistogram



