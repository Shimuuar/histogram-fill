{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- |
-- Module     : Data.Histogram.Bin
-- Copyright  : Copyright (c) 2010, Alexey Khudyakov <alexey.skladnoy@gmail.com>
-- License    : BSD3
-- Maintainer : Alexey Khudyakov <alexey.skladnoy@gmail.com>
-- Stability  : experimental
--
-- Extra binning algorithms

module Data.Histogram.Bin.Extra ( Enum2D(..)
                                , BinEnum2D
                                , binEnum2D
                                , BinPermute(permutedBin, permuteTo, permuteFrom)
                                , permuteByTable
                                , permuteBin
                                ) where

import Control.Applicative
import Control.Monad --  (forM_,liftM2)

import qualified Data.Vector.Generic         as G
import qualified Data.Vector.Unboxed         as U
import qualified Data.Vector.Unboxed.Mutable as M
import           Data.Vector.Generic            ((!))
import Text.Read

import Data.Histogram.Bin
import Data.Histogram.Parse

----------------------------------------------------------------

-- | Type class very similar to 'Enum' but elements of type are
--   enumerated on 2-dimensional grid
class Enum2D a where
  -- | Convert value to index
  fromEnum2D :: a -> (Int,Int)
  -- | Convert index to value
  toEnum2D :: (Int,Int) -> a

instance (Enum a, Enum b) => Enum2D (a,b) where
  fromEnum2D (x,y) = (fromEnum x, fromEnum y)
  toEnum2D   (i,j) = (toEnum   i, toEnum   j)



----------------------------------------------------------------
-- 2D enumaration bin
----------------------------------------------------------------

-- | Binning for 2D enumerations
newtype BinEnum2D i = BinEnum2D (Bin2D BinI BinI)

-- | Construct indexed bin
binEnum2D :: Enum2D i => i -> i -> BinEnum2D i
binEnum2D lo hi = let (ix,iy) = fromEnum2D lo
                      (jx,jy) = fromEnum2D hi
                  in BinEnum2D $ BinI ix jx >< BinI iy jy

instance Enum2D i => Bin (BinEnum2D i) where
    type BinValue (BinEnum2D i) = i
    toIndex   !(BinEnum2D b) !x = toIndex b (fromEnum2D x)
    fromIndex !(BinEnum2D b) !i = toEnum2D  (fromIndex b i)
    inRange   !(BinEnum2D b) !x = inRange b (fromEnum2D x)
    nBins     !(BinEnum2D b)    = nBins b

instance (Show i, Enum2D i) => Show (BinEnum2D i) where
    show (BinEnum2D b) = unlines [ "# BinEnum2D"
                                 , "# Low  = " ++ show (toEnum2D (fromIndex b 0            ) :: i)
                                 , "# High = " ++ show (toEnum2D (fromIndex b (nBins b - 1)) :: i)
                                 ]
instance (Read i, Enum2D i) => Read (BinEnum2D i) where
    readPrec = do
      keyword "BinEnum2D"
      liftM2 binEnum2D (value "Low") (value "High")


----------------------------------------------------------------
-- Permutation
----------------------------------------------------------------

-- | Direct permutation of indices.
data BinPermute b = BinPermute { permutedBin :: b            -- ^ Original bin
                               , permuteTo   :: U.Vector Int -- ^ Maps original bin's indices to new indices
                               , permuteFrom :: U.Vector Int -- ^ Inverse of pervious table
                               }

instance Bin b => Bin (BinPermute b) where
  type BinValue (BinPermute b) = BinValue b
  toIndex   (BinPermute b to _)   !x = to ! toIndex b x
  fromIndex (BinPermute b _ from) !i = fromIndex b (from ! i)
  inRange   (BinPermute b _ _)     x = inRange b x
  nBins = nBins . permutedBin

instance (Bin1D b) => Bin1D (BinPermute b) where
  lowerLimit = lowerLimit . permutedBin
  upperLimit = upperLimit . permutedBin
  binsList (BinPermute b _ a) = res
    where
      res = G.generate (nBins b) fun
      arr = binsList b `asTypeOf` res
      fun i = arr ! (a ! i)
  binsListRange (BinPermute b _ a) = res
    where
      res = G.generate (nBins b) fun
      arr = binsListRange b `asTypeOf` res
      fun i = arr ! (a ! i)
  {-# INLINE binsList      #-}
  {-# INLINE binsListRange #-}

instance VariableBin1D b => VariableBin1D (BinPermute b) where
  binSizeN b i = binSizeN (permutedBin b) (permuteFrom b ! i)
  
instance UniformBin1D b => UniformBin1D (BinPermute b) where
  binSize = binSize . permutedBin
  

instance Show b => Show (BinPermute b) where
  show (BinPermute b to _) = unlines [ "# BinPermute"
                                     , "# Permutation = " ++ show (U.toList to)
                                     ] ++ show b

instance Read BinI => Read (BinPermute BinI) where
  readPrec = do keyword "BinPermute"
                to   <- U.fromList <$> value "Permutation"
                b    <- readPrec
                from <- case checkPermutationTable b (invertPermutationTable to) of
                          Just v  -> return v
                          Nothing -> fail "Invalid permutation"
                return $ BinPermute b to from


-- Check whether this viable permutation
checkPermutationTable :: Bin b => b -> U.Vector Int -> Maybe (U.Vector Int)
checkPermutationTable b v = do
  let n      = U.length v
      good i = i >= 0 && i < n
  guard $ nBins b == n
  guard $ U.all good v
  return v


-- Calculate inverse permutation
invertPermutationTable :: U.Vector Int -> U.Vector Int
invertPermutationTable v = U.create $ do a <- M.newWith n (-1)
                                         forM_ [0..n-1] (writeInvert a)
                                         return a
  where
    n = U.length v
    writeInvert a i | j >= 0 && j < n = M.write a j i
                    | otherwise       = return ()
                      where j = v ! i


-- | Constuct bin permutation from table
permuteByTable :: Bin b => b -> U.Vector Int -> Maybe (BinPermute b)
permuteByTable b tbl = BinPermute b <$>
                       checkPermutationTable b tbl <*>
                       checkPermutationTable b (invertPermutationTable tbl)


-- | Constuct bin permutation from function.
permuteBin :: Bin b => b -> (Int -> Int) -> Maybe (BinPermute b)
permuteBin b f = BinPermute b <$>
                 checkPermutationTable b to <*>
                 checkPermutationTable b (invertPermutationTable to)
    where
      to   = U.map f $ U.enumFromN 0 (nBins b)

