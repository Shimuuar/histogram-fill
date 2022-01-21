{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE Rank2Types   #-}
-- |
-- Module     : Data.Histogram.Fill
-- Copyright  : Copyright (c) 2009, Alexey Khudyakov <alexey.skladnoy@gmail.com>
-- License    : BSD3
-- Maintainer : Alexey Khudyakov <alexey.skladnoy@gmail.com>
-- Stability  : experimental
--
-- Stateful and pure (still stateful under the hood) accumulators. 
--
module Data.Histogram.Fill ( 
    -- * Builder type class
    HistBuilder(..)
    -- ** Operators
  , (<<-)
  , (<<?)
  , (<<-$)
  , (-<<)
    -- * Histogram builders
    -- ** Stateful
  , HBuilderM(..)
  , feedOne
  , freezeHBuilderM
    -- ** Stateless
  , HBuilder(HBuilder)
  , toHBuilderST
  , toHBuilderIO
  , toHBuilderM
    -- * Histogram constructors
    -- ** Using unboxed vectors
  , module Data.Histogram.Bin
  , mkSimple
  , mkWeighted
  , mkMonoidal
  , mkFoldBuilder
    -- ** Using generic vectors
  , mkSimpleG
  , mkWeightedG
  , mkMonoidalG
  , mkFoldBuilderG
    -- ** Pure fold
  , mkFolder
    -- ** Generic constructors
  , mkStatefulBuilder
    -- * Fill histograms
  , fillBuilder
  , fillBuilderVec
    -- * Auxillary functions
    -- $auxillary
  , forceInt
  , forceDouble
  , forceFloat
    -- * Examples
    -- $examples
  ) where

import Control.Applicative
import Control.Monad.ST
import Control.Monad.Primitive
import Control.Lens.Fold

import Data.Monoid
import Data.Profunctor
import Data.Vector.Unboxed    (Unbox)
import Data.Primitive.MutVar
import qualified Data.Vector.Generic as G
import qualified Data.Foldable       as F
import qualified Data.Traversable    as F

import Data.Histogram
import qualified Data.Histogram.Generic as H
import Data.Histogram.Bin
import Data.Histogram.ST



----------------------------------------------------------------
-- Type class
----------------------------------------------------------------

-- | Type class for stateful accumulators. In this module they are
--   called builders. Every builder is parametrized by two
--   types. First one is type of values which are fed to accumulator
--   and second one is type of values which could be extracted from
--   it.
--
--   Every instance of 'HBuilder' should be instance of 'Functor' too
--   and satisfy 'fmap' == 'modifyOut'.
class Profunctor h => HistBuilder h where
  transformInput :: Fold a b -> h b c -> h a c

-- | Apply function to output of histogram.
modifyOut :: HistBuilder h => (b -> b') -> h a b -> h a  b'
modifyOut = rmap

-- | Change input of builder by applying function to it.
modifyIn :: HistBuilder h => (a' -> a) -> h a b -> h a' b
modifyIn = lmap


-- | Modify input of builder
(<<-) :: HistBuilder h => h a b -> (a' -> a) -> h a' b
(<<-) = flip modifyIn
{-# INLINE (<<-) #-}

-- | Add cut for input
(<<?) :: HistBuilder h => h a b -> (a -> Bool) -> h a b
h <<? f = transformInput (filtered f) h
{-# INLINE (<<?) #-}

-- | Apply function which modify builder
(<<-$) :: h a b -> (h a b -> h a' b) -> h a' b
h <<-$ f = f h
{-# INLINE (<<-$) #-}

-- | Modify output of histogram. In fact it's same as '<$>' but have opposite fixity
(-<<) :: HistBuilder h => (b -> b') -> h a b -> h a b'
(-<<) = modifyOut
{-# INLINE (-<<) #-}

-- Fixity of operators
infixl 5 <<-
infixl 5 <<?
infixl 5 <<-$
infixr 4 -<<


-- $examples
--
-- All examples will make use of operators to create builders. It's
-- possible to avoid their use, but operators offer clear notation and
-- compose nicely in a pipeline. Also note that data flows from right to
-- left as with the '.' operator.
--
-- First example just counts ints in the [0..4] inclusive range.
-- 'fillBuilder' is used to put all values into an accumulator.
--
-- > ghci> let h = forceInt -<< mkSimple (BinI 0 4)
-- > ghci> fillBuilder h [0,0,0,1,1,2,3,4,4,4]
-- > # Histogram
-- > # Underflows = 0
-- > # Overflows  = 0
-- > # BinI
-- > # Low  = 0
-- > # High = 4
-- > 0       3
-- > 1       2
-- > 2       1
-- > 3       1
-- > 4       3
--
-- More involved example that only accepts even numbers. Filtering could be
-- achieved with either 'addCut' or the '<<?' operator.
--
-- > forceInt -<< mkSimple (BinI 0 4) <<? even
--
-- Although for example above same result could be achieved by
-- filtering of input, it doesn't work when multiple histograms with
-- different cuts are filled simultaneously.
--
-- Next example illustrates the use of an applicative interface. Here
-- two histograms are filled at the same time. First accept only even
-- numbers and second only the odd ones. Results are put into the tuple.
--
-- > (,) <$> 
-- >   (forceInt -<< mkSimple (BinI 0 4) <<? even)
-- >   (forceInt -<< mkSimple (BinI 0 4) <<? odd)
--
-- Another approach is to use 'F.sequenceA' to simultaneously fill
-- a list (or any other 'Traversable'). 
--
-- > Data.Traversable.sequenceA [
-- >     forceInt -<< mkSimple (BinI 0 4) <<? even
-- >   , forceInt -<< mkSimple (BinI 0 4) <<? odd
-- >   ]
--
-- If one wants to collect results from many histograms he can take an
-- advantage of the 'Monoid' instance of 'HBuilder'. Example below
-- concatenates string outputs of individual histograms.
--
-- > mconcat [
-- >     show . forceInt -<< mkSimple (BinI 0 4) <<? even
-- >   , show . forceInt -<< mkSimple (BinI 0 4) <<? odd
-- >   ]


----------------------------------------------------------------
-- Monadic builder
----------------------------------------------------------------

-- | Stateful histogram builder. Adding a value to builder could be done
--   with 'feedOne' and the result could be extracted with
--   'freezeHBuilderM'.
--
--   There are two ways to obtain a stateful builder. First and
--   recommended way is to thaw 'HBuilder' using 'toHBuilderIO' or
--   'toHBuilderST'. Second possibility is to use 'mkStatefulBuilder'.
data HBuilderM m a b = HBuilderM { hbInput  :: a -> m ()
                                 , hbOutput :: m b
                                 }
instance Functor m => Profunctor (HBuilderM m) where
  lmap f h = h { hbInput  = hbInput h . f }
  rmap f h = h { hbOutput = f <$> hbOutput h }

-- | Builders modified using 'HistBuilder' API will share the same buffer.
instance Applicative m => HistBuilder (HBuilderM m) where
  {-# INLINE transformInput #-}
  transformInput l h = h { hbInput = transformFun l (hbInput h) }

transformFun :: Applicative m => Fold a b -> (b -> m ()) -> (a -> m ())
transformFun l f = getAp . getConst . l (Const . Ap . f)

instance Functor m => Functor (HBuilderM m a) where
    fmap = rmap
instance Applicative m => Applicative (HBuilderM m a) where
    pure x = HBuilderM { hbInput  = const $ pure ()
                       , hbOutput = pure x
                       }
    f <*> g = HBuilderM { hbInput  = \a -> hbInput f a *> hbInput g a
                        , hbOutput = liftA2 ($) (hbOutput f) (hbOutput g)
                        }

instance (Monad m, Semigroup b) => Semigroup (HBuilderM m a b) where
    (<>) = liftA2 (<>)
    {-# INLINE (<>) #-}

instance (Monad m, Monoid b) => Monoid (HBuilderM m a b) where
    mempty = HBuilderM { hbInput  = \_ -> return ()
                       , hbOutput = return mempty
                       }
    mappend = liftA2 mappend
    mconcat = fmap mconcat . F.sequenceA
    {-# INLINE mempty  #-}
    {-# INLINE mappend #-}
    {-# INLINE mconcat #-}


-- | Put one item into the histogram
feedOne :: HBuilderM m a b -> a -> m ()
feedOne = hbInput
{-# INLINE feedOne #-}

-- | Extract the result from a histogram builder. It's safe to call
--   this function multiple times, and mutate the builder afterwards.
freezeHBuilderM :: HBuilderM m a b -> m b
freezeHBuilderM = hbOutput
{-# INLINE freezeHBuilderM #-}



----------------------------------------------------------------
-- Stateless
----------------------------------------------------------------

-- | Wrapper around the stateful histogram builder. It is much more
--   convenient to work with this one than with 'HBuilderM'.
newtype HBuilder a b = HBuilder (forall s. ST s (HBuilderM (ST s) a b))

-- | Convert the builder to a stateful builder in a primitive monad
toHBuilderM :: PrimMonad m => HBuilder a b -> m (HBuilderM m a b)
{-# INLINE toHBuilderM #-}
toHBuilderM (HBuilder hb) = do
  HBuilderM inp out <- stToPrim hb
  pure $ HBuilderM (stToPrim . inp) (stToPrim out)

-- | Convert the builder to stateful builder in the ST monad
toHBuilderST :: HBuilder a b -> ST s (HBuilderM (ST s) a b)
{-# INLINE toHBuilderST #-}
toHBuilderST = toHBuilderM

-- | Convert the builder to builder in the IO monad
toHBuilderIO :: HBuilder a b -> IO (HBuilderM IO a b)
{-# INLINE toHBuilderIO #-}
toHBuilderIO = toHBuilderM

instance Profunctor HBuilder where
  lmap f (HBuilder h) = HBuilder (lmap f <$> h)
  rmap f (HBuilder h) = HBuilder (rmap f <$> h)

instance HistBuilder HBuilder where
  transformInput l (HBuilder h) = HBuilder (transformInput l <$> h)
  {-# INLINE transformInput #-}

instance Functor (HBuilder a) where
    fmap = modifyOut
instance Applicative (HBuilder a) where
    pure x  = HBuilder (return $ pure x)
    (HBuilder f) <*> (HBuilder g) = HBuilder $ liftA2 (<*>) f g
instance Semigroup b => Semigroup (HBuilder a b) where
    (<>) = liftA2 (<>)
    {-# INLINE (<>) #-}
instance Monoid b => Monoid (HBuilder a b) where
    mempty  = HBuilder (return mempty)
    mappend = liftA2 mappend
    mconcat = fmap mconcat . F.sequenceA
    {-# INLINE mempty  #-}
    {-# INLINE mappend #-}
    {-# INLINE mconcat #-}



----------------------------------------------------------------
-- Constructors
----------------------------------------------------------------

-- | Create builder. Bin content will be incremented by 1 for each
--   item put into the histogram
mkSimple :: (Bin bin, Unbox val, Num val
            ) => bin -> HBuilder (BinValue bin) (Histogram bin val)
mkSimple = mkSimpleG
{-# INLINE mkSimple #-}

-- | Create builder. Bin content will be incremented by the weight supplied
--   for each item put into the histogram
mkWeighted :: (Bin bin, Unbox val, Num val
              ) => bin -> HBuilder (BinValue bin,val) (Histogram bin val)
mkWeighted = mkWeightedG
{-# INLINE mkWeighted #-}

-- | Create builder. New value will be mappended to current content of
--   a bin for each item put into the histogram
mkMonoidal :: (Bin bin, Unbox val, Monoid val
              ) => bin -> HBuilder (BinValue bin,val) (Histogram bin val)
mkMonoidal = mkMonoidalG
{-# INLINE mkMonoidal #-}

-- | Create a most generic histogram builder.
mkFoldBuilder :: (Bin bin, Unbox val)
              => bin               -- ^ Binning algorithm
              -> val               -- ^ Initial value
              -> (val -> a -> val) -- ^ Folding function
              -> HBuilder (BinValue bin, a) (Histogram bin val)
{-# INLINE mkFoldBuilder #-}
mkFoldBuilder = mkFoldBuilderG



-- | Create builder. Bin content will be incremented by 1 for each
--   item put into the histogram
mkSimpleG :: (Bin bin, G.Vector v val, Num val
            ) => bin -> HBuilder (BinValue bin) (H.Histogram v bin val)
mkSimpleG bin = HBuilder $ do
  acc <- newMHistogram 0 bin
  return HBuilderM { hbInput  = \x -> fill acc x (+) 1
                   , hbOutput = freezeHist acc
                   }
{-# INLINE mkSimpleG #-}

-- | Create builder. Bin content will incremented by the weight supplied
--   for each item put into the histogram
mkWeightedG :: (Bin bin, G.Vector v val, Num val
              ) => bin -> HBuilder (BinValue bin,val) (H.Histogram v bin val)
mkWeightedG bin = mkFoldBuilderG bin 0 (+)
{-# INLINE mkWeightedG #-}

-- | Create builder. New value will be mappended to current content of
--   a bin for each item put into the histogram
mkMonoidalG :: (Bin bin, G.Vector v val, Monoid val
              ) => bin -> HBuilder (BinValue bin,val) (H.Histogram v bin val)
mkMonoidalG bin = mkFoldBuilderG bin mempty mappend
{-# INLINE mkMonoidalG #-}

-- | Create most generic histogram builder.
mkFoldBuilderG :: (Bin bin, G.Vector v val)
               => bin               -- ^ Binning algorithm
               -> val               -- ^ Initial value
               -> (val -> a -> val) -- ^ Folding function
               -> HBuilder (BinValue bin, a) (H.Histogram v bin val)
{-# INLINE mkFoldBuilderG #-}
mkFoldBuilderG bin x0 f = HBuilder $ do
  acc <- newMHistogram x0 bin
  return HBuilderM { hbInput  = \(!x,!w) -> fill acc x f w
                   , hbOutput = freezeHist acc
                   }


-- | Create histogram builder which just does ordinary pure fold. It
--   is intended for use when some fold should be performed together
--   with histogram filling.
mkFolder :: b -> (a -> b -> b) -> HBuilder a b
{-# INLINE mkFolder #-}
mkFolder a f = HBuilder $ do
  ref <- newMutVar a
  return HBuilderM { hbInput  = \aa -> do b <- readMutVar ref
                                          writeMutVar ref $! f aa b
                   , hbOutput = readMutVar ref
                   }


-- | Create stateful histogram builder. The output function should be safe
--   to call multiple times and the builder could be modified afterwards.
--   So functions like @unsafeFreeze@ from @vector@ couldn't be used.
mkStatefulBuilder :: (a -> m ()) -- ^ Add value to accumulator
                  -> m b         -- ^ Extract result from accumulator
                  -> HBuilderM m a b
{-# INLINE mkStatefulBuilder #-}
mkStatefulBuilder = HBuilderM



----------------------------------------------------------------
-- Actual filling of histograms
----------------------------------------------------------------

-- | Fill histogram builder.
fillBuilder :: F.Foldable f => HBuilder a b -> f a -> b
fillBuilder hb xs =
    runST $ do h <- toHBuilderST hb
               F.mapM_ (feedOne h) xs
               freezeHBuilderM h

-- | Fill histogram builder.
fillBuilderVec :: G.Vector v a => HBuilder a b -> v a -> b
{-# INLINE fillBuilderVec #-}
fillBuilderVec hb vec =
    runST $ do h <- toHBuilderST hb
               G.mapM_ (feedOne h) vec
               freezeHBuilderM h

----------------------------------------------------------------

-- $auxillary
--
-- In some cases the builder constructors do not constrain the output type
-- enough. The output type is still parametric in value type of histogram.
-- Functions below are just the 'id' function with a more restrictive
-- signature.
--
-- In example below 'forceInt' used to fix type of the histogram to
-- 'Histogram BinI Int'. Without it, the compiler cannot infer type of
-- the intermediate histogram.
--
-- > show . forceInt -<< mkSimple (BinI 1 10)

forceInt :: H.Histogram v bin Int -> H.Histogram v bin Int
forceInt = id

forceDouble :: H.Histogram v bin Double -> H.Histogram v bin Double
forceDouble = id

forceFloat :: H.Histogram v bin Float -> H.Histogram v bin Float
forceFloat = id
