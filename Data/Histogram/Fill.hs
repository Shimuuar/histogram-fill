{-# LANGUAGE Rank2Types #-}
-- |
-- Module     : Data.Histogram.Fill
-- Copyright  : Copyright (c) 2009, Alexey Khudyakov <alexey.skladnoy@gmail.com>
-- License    : BSD3
-- Maintainer : Alexey Khudyakov <alexey.skladnoy@gmail.com>
-- Stability  : experimental
--
-- Module with algorithms for histogram filling. This is pure wrapper
-- around stateful histograms.
--
module Data.Histogram.Fill ( -- * Histogram builders API
                             HistBuilder(..)
                           , (<<-)
                           , (<<?)
                           , (<<-<)
                           , (<<~)
                           , (-<<)
                             -- * Histogram builders
                             -- ** Stateful
                           , HBuilderM
                           , feedOne
                           , freezeHBuilderM
                           , joinHBuilderM
                           , joinHBuilderMonoidM
                           , treeHBuilderM
                           , treeHBuilderMonoidM
                             -- ** Stateless
                           , HBuilder
                           , joinHBuilder
                           , joinHBuilderMonoid
                           , treeHBuilder
                           , treeHBuilderMonoid
                             -- * Fill histograms
                           , fillBuilder
                             -- * Histogram constructors
                           , module Data.Histogram.Bin
                           , mkSimple
                           , mkWeighted
                           , mkMonoidal
                             -- * Auxillary functions
                           , forceInt
                           , forceDouble
                           , forceFloat
                           ) where

import Control.Applicative
import Control.Monad       (when,liftM,liftM2)
import Control.Monad.ST 
import Control.Monad.Primitive

import Data.Monoid         (Monoid(..))
import Data.Vector.Unboxed (Unbox)

import Data.Histogram
import Data.Histogram.Bin
import Data.Histogram.ST

----------------------------------------------------------------
-- Type class
----------------------------------------------------------------

-- | Histogram builder typeclass. Instance of this class contain
--   instructions how to build histograms.
class HistBuilder h where
    -- | Convert output of histogram
    modifyOut   :: (b -> b') -> h a b -> h a  b'
    -- | Convert input type of histogram from a to a'
    modifyIn    :: (a' -> a) -> h a b -> h a' b
    -- | Make input function accept value only if it's Just a.
    modifyMaybe :: h a b -> h (Maybe a) b
    -- | Make ability input function accept list
    modifyList :: h a b -> h [a] b
    -- | Add cut to histogram. Only put value histogram if condition is true.
    addCut      :: (a -> Bool) -> h a b -> h a b

-- | Modify input of builder 
(<<-) :: HistBuilder h => h a b -> (a' -> a) -> h a' b
(<<-) = flip modifyIn
infixl 5 <<-

-- | Modify input of builder 
(<<~) :: HistBuilder h => h a b -> (a' -> Maybe a) -> h a' b
h <<~ f = modifyMaybe h <<- f
infixl 5 <<~

-- | Modify input of builder 
(<<-<) :: HistBuilder h => h a b -> (a' -> [a]) -> h a' b
h <<-< f = modifyList h <<- f
infixl 5 <<-<

-- | Add cut for input
(<<?) :: HistBuilder h => h a b -> (a -> Bool) -> h a b
(<<?) = flip addCut
infixl 5 <<?

-- | Modify output of histogram. In fact it's same as '<$>' but have opposite fixity
(-<<) :: HistBuilder h => (b -> b') -> h a b -> h a b'
(-<<) = modifyOut
infixr 4 -<<


----------------------------------------------------------------
-- ST based builder
----------------------------------------------------------------

-- | Stateful histogram builder.
data HBuilderM m a b = HBuilderM { hbInput  :: a -> m ()
                                 , hbOutput :: m b
                                 }

instance PrimMonad m => HistBuilder (HBuilderM m) where
    modifyIn  f h = h { hbInput  = hbInput h . f }
    addCut    f h = h { hbInput  = \x -> when (f x) (hbInput h x) }
    modifyMaybe h = h { hbInput  = modified } 
        where modified (Just x) = hbInput h x
              modified Nothing  = return ()
    modifyList h = h { hbInput = modified }
        where modified = mapM_ (hbInput h)
    modifyOut f h = h { hbOutput = f `liftM` hbOutput h }

instance PrimMonad m => Functor (HBuilderM m a) where
    fmap = modifyOut
instance PrimMonad m => Applicative (HBuilderM m a) where
    pure x = HBuilderM { hbInput  = const $ return ()
                       , hbOutput = return x
                       }
    f <*> g = HBuilderM { hbInput  = \a -> hbInput f a >> hbInput g a
                        , hbOutput = do a <- hbOutput f
                                        b <- hbOutput g
                                        return (a b)
                        }
                                        
-- | Put one value into histogram
feedOne :: PrimMonad m => HBuilderM m a b -> a -> m ()
feedOne = hbInput
{-# INLINE feedOne #-}

-- | Create stateful histogram from instructions. Histograms could
--   be filled either in the ST monad or with createHistograms
freezeHBuilderM :: PrimMonad m => HBuilderM m a b -> m b
freezeHBuilderM = hbOutput
{-# INLINE freezeHBuilderM #-}

-- | Join list of builders into one builder
joinHBuilderM :: PrimMonad m => [HBuilderM m a b] -> HBuilderM m a [b]
joinHBuilderM hs = HBuilderM { hbInput  = \x -> mapM_ (flip hbInput x) hs
                             , hbOutput = mapM hbOutput hs
                             }
{-# INLINE joinHBuilderM #-}

-- | Join list of builders into one builders
joinHBuilderMonoidM :: (PrimMonad m, Monoid b) => [HBuilderM m a b] -> HBuilderM m a b
joinHBuilderMonoidM = fmap mconcat . joinHBuilderM
{-# INLINE joinHBuilderMonoidM #-}

treeHBuilderM :: PrimMonad m => [HBuilderM m a b -> HBuilderM m a' b'] -> HBuilderM m a b -> HBuilderM m a' [b']
treeHBuilderM fs h = joinHBuilderM $ map ($ h) fs
{-# INLINE treeHBuilderM #-}

treeHBuilderMonoidM :: (PrimMonad m, Monoid b') => 
                        [HBuilderM m a b -> HBuilderM m a' b'] -> HBuilderM m a b -> HBuilderM m a' b'
treeHBuilderMonoidM fs h = joinHBuilderMonoidM $ map ($ h) fs
{-# INLINE treeHBuilderMonoidM #-}


----------------------------------------------------------------
-- Stateless 
----------------------------------------------------------------

-- | Stateless histogram builder
newtype HBuilder a b = HBuilder { toBuilderM :: (forall s . ST s (HBuilderM (ST s) a b)) }

instance HistBuilder (HBuilder) where
    modifyIn  f (HBuilder h) = HBuilder (modifyIn  f <$> h)
    addCut    f (HBuilder h) = HBuilder (addCut    f <$> h)
    modifyMaybe (HBuilder h) = HBuilder (modifyMaybe <$> h)
    modifyList  (HBuilder h) = HBuilder (modifyList  <$> h)
    modifyOut f (HBuilder h) = HBuilder (modifyOut f <$> h)

instance Functor (HBuilder a) where
    fmap = modifyOut
instance Applicative (HBuilder a) where
    pure x  = HBuilder (return $ pure x)
    (HBuilder f) <*> (HBuilder g) = HBuilder $ liftM2 (<*>) f g 

-- | Join list of builders
joinHBuilder :: [HBuilder a b] -> HBuilder a [b]
joinHBuilder hs = HBuilder (joinHBuilderM <$> mapM toBuilderM hs)
{-# INLINE joinHBuilder #-}

-- | Join list of builders
joinHBuilderMonoid :: Monoid b => [HBuilder a b] -> HBuilder a b
joinHBuilderMonoid = modifyOut mconcat . joinHBuilder
{-# INLINE joinHBuilderMonoid #-}

treeHBuilder :: [HBuilder a b -> HBuilder a' b'] -> HBuilder a b -> HBuilder a' [b']
treeHBuilder fs h = joinHBuilder $ map ($ h) fs
{-# INLINE treeHBuilder #-}

treeHBuilderMonoid :: Monoid b' => [HBuilder a b -> HBuilder a' b'] -> HBuilder a b -> HBuilder a' b'
treeHBuilderMonoid fs h = joinHBuilderMonoid $ map ($ h) fs
{-# INLINE treeHBuilderMonoid #-}

----------------------------------------------------------------
-- Actual filling of histograms
----------------------------------------------------------------

fillBuilder :: HBuilder a b -> [a] -> b
fillBuilder hb xs = 
    runST $ do h <- toBuilderM hb
               mapM_ (feedOne h) xs
               freezeHBuilderM h


mkSimple :: (Bin bin, Unbox val, Num val
            ) => bin -> HBuilder (BinValue bin) (Histogram bin val)
mkSimple bin = 
  HBuilder $ do acc <- newMHistogram 0 bin
                return $ HBuilderM { hbInput  = fillOne acc
                                   , hbOutput = freezeHist acc
                                   }
{-# INLINE mkSimple #-}

mkWeighted :: (Bin bin, Unbox val, Num val
              ) => bin -> HBuilder (BinValue bin,val) (Histogram bin val)
mkWeighted bin = HBuilder $ do acc <- newMHistogram 0 bin
                               return $ HBuilderM { hbInput  = fillOneW acc
                                                  , hbOutput = freezeHist acc
                                                  }
{-# INLINE mkWeighted #-}

mkMonoidal :: (Bin bin, Unbox val, Monoid val
              ) => bin -> HBuilder (BinValue bin,val) (Histogram bin val)
mkMonoidal bin = HBuilder $ do acc <- newMHistogram mempty bin
                               return $ HBuilderM { hbInput  = fillMonoid acc
                                                  , hbOutput = freezeHist acc
                                                  }
{-# INLINE mkMonoidal #-}

----------------------------------------------------------------

-- | Function used to restrict type of histrogram.
forceInt :: Histogram bin Int -> Histogram bin Int
forceInt = id

-- | Function used to restrict type of histrogram.
forceDouble :: Histogram bin Double -> Histogram bin Double
forceDouble = id

-- | Function used to restrict type of histrogram.
forceFloat :: Histogram bin Float -> Histogram bin Float
forceFloat = id
