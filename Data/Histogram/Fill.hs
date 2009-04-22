{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module Data.Histogram.Fill ( HBuilderCl
                           , HBuilder
                           , HistBuilder
                           , builderList

                           , createHistograms
                           ) where

import Data.Monoid


import Data.Histogram.Internal.Accumulator
----------------------------------------------------------------

createHistograms :: Monoid b => HBuilder a b -> [a] -> b
createHistograms h xs = fillHistograms (runBuilder h) xs

----------------------------------------------------------------

-- | Histogram builder typeclass. Value of this type contain instructions
--   how to build histograms.
class HBuilderCl h where 
    modifyIn  :: (a' -> a) -> h a b -> h a' b 
    modifyOut :: (b -> b') -> h a b -> h a  b'
    runBuilder :: h a b -> HistogramST s a b


-- | Existential type. Histogram builder. 
data HBuilder a b = forall h . HBuilderCl h => MkHBuilder (h a b)

instance HBuilderCl HBuilder where 
    modifyIn  f (MkHBuilder h) = MkHBuilder $ modifyIn f h
    modifyOut g (MkHBuilder h) = MkHBuilder $ modifyOut g h 
    runBuilder (MkHBuilder h)  = runBuilder h


-- List of histograms 
newtype HBuilderList a b = HBuilderList [HBuilder a b]

-- | Wrap list of histogram builders into HBuilder existential
builderList :: [HBuilder a b] -> HBuilder a b
builderList = MkHBuilder . HBuilderList

instance HBuilderCl HBuilderList where
    modifyIn  f (HBuilderList l) = HBuilderList $ map (modifyIn f) l
    modifyOut g (HBuilderList l) = HBuilderList $ map (modifyOut g) l
    runBuilder (HBuilderList l)  = accumList $ map runBuilder l

-- 
data HistBuilder ix v a b = HistBuilder { histRange :: (ix,ix)
                                        , histIn    :: a -> [ix]
                                        , histOut   :: (v,[(ix, v)],v) -> b
                                        }


modifyInF :: (a' -> a) -> HistBuilder ix v a b -> HistBuilder ix v a' b
modifyInF  f h = h { histIn  = histIn h . f }
modifyOutF :: (b -> b') -> HistBuilder ix v a b -> HistBuilder ix v a b'
modifyOutF g h = h { histOut = g . histOut h }
runBuilderF  h = accumHist (histIn h) (histOut h) (histRange h)

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
