{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable  #-}
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

import Control.Applicative ((<$>), Applicative(..))
import Control.Monad       (forM_,liftM2, guard)
import Control.Monad.ST    (ST)
import Control.DeepSeq     (NFData(..)

import qualified Data.Vector.Unboxed         as U
import qualified Data.Vector.Unboxed.Mutable as M
import           Data.Vector.Generic            ((!))
import Data.Data          (Data,Typeable)
import Text.Read          (Read(..))         

import Data.Histogram.Bin
import Data.Histogram.Bin.Read

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
                      deriving (Eq,Data,Typeable)

-- | Construct indexed bin
binEnum2D :: Enum2D i => i -> i -> BinEnum2D i
binEnum2D lo hi = let (ix,iy) = fromEnum2D lo
                      (jx,jy) = fromEnum2D hi
                  in BinEnum2D $ binI ix jx >< binI iy jy

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

instance NFData (BinEnum2D i) where
  rnf b = b `seq` ()

----------------------------------------------------------------
-- Permutation
----------------------------------------------------------------

-- | Direct permutation of indices.
data BinPermute b = BinPermute { permutedBin :: b            -- ^ Original bin
                               , permuteTo   :: U.Vector Int -- ^ Maps original bin's indices to new indices
                               , permuteFrom :: U.Vector Int -- ^ Inverse of pervious table
                               }
                    deriving (Eq,Data,Typeable)

instance Bin b => Bin (BinPermute b) where
  type BinValue (BinPermute b) = BinValue b
  toIndex   (BinPermute b to _)   !x = to ! toIndex b x
  fromIndex (BinPermute b _ from) !i = fromIndex b (from ! i)
  inRange   (BinPermute b _ _)       = inRange b
  nBins = nBins . permutedBin

instance IntervalBin b => IntervalBin (BinPermute b) where
  binInterval b i = binInterval (permutedBin b) (permuteFrom b ! i)

instance VariableBin b => VariableBin (BinPermute b) where
  binSizeN b i = binSizeN (permutedBin b) (permuteFrom b ! i)
  
instance UniformBin b => UniformBin (BinPermute b) where
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

instance NFData b => NFData (BinPermute b) where
  rnf (BinPermute b va vb) = rnf b `seq` va `seq` vb `seq` ()

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
invertPermutationTable v = U.create $ do a <- M.replicate n (-1)
                                         forM_ [0..n-1] (writeInvert a)
                                         return a
  where
    n = U.length v
    writeInvert :: M.MVector s Int -> Int -> ST s ()
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
