{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# OPTIONS_GHC -Wno-orphans #-}
-- |
module Data.Histogram.Serialise
  () where

import Codec.Serialise
import Codec.Serialise.Encoding
import Codec.Serialise.Decoding
import qualified Data.Vector.Generic as G

import Data.Histogram.Bin
import Data.Histogram.Bin.MaybeBin
import Data.Histogram.Bin.BinVar
import Data.Histogram.Generic (Histogram, histogramUO, histData, outOfRange, bins)


instance Serialise BinI where
  decode = decodeProd2 binI
  encode = encodeProd2 lowerLimit upperLimit

instance Serialise BinInt where
  decode = decodeProd3 binIntStep
  encode = encodeProd3 lowerLimit binSize nBins

instance (RealFrac f, Serialise f) => Serialise (BinF f) where
  decode = decodeProd3 binFstep
  encode = encodeProd3 lowerLimit binSize nBins

instance Serialise BinD where
  decode = decodeProd3 binDstep
  encode = encodeProd3 lowerLimit binSize nBins

instance Serialise LogBinD where
  decode = decodeProd3 logBinDN
  encode = encodeProd3 lowerLimit logBinDIncrement nBins

instance (Serialise bX, Serialise bY) => Serialise (Bin2D bX bY) where
  decode = decodeProd2 Bin2D
  encode = encodeProd2 binX binY

deriving newtype instance Serialise (BinEnum a)
deriving newtype instance Serialise bin => Serialise (MaybeBin bin)
-- FIXME: questionable instance. Doesn't check stuff
deriving newtype instance (Serialise (v a)) => Serialise (BinVarG v a)

instance (Serialise a, Serialise bin, Serialise (v a), G.Vector v a, Bin bin
         ) => Serialise (Histogram v bin a) where
  decode = decodeProd3 histogramUO
  encode = encodeProd3 bins outOfRange histData


----------------------------------------------------------------
-- Helpers
----------------------------------------------------------------

encodeProd2 :: (Serialise a, Serialise b) => (r -> a) -> (r -> b) -> r -> Encoding
encodeProd2 a b r
  =  encodeListLen 2
  <> encode (a r)
  <> encode (b r)

encodeProd3
  :: (Serialise a, Serialise b, Serialise c)
  => (r -> a) -> (r -> b) -> (r -> c) -> r -> Encoding
encodeProd3 a b c r
  =  encodeListLen 3
  <> encode (a r)
  <> encode (b r)
  <> encode (c r)

decodeProd2 :: (Serialise a, Serialise b) => (a -> b -> r) -> Decoder s r
decodeProd2 f = do
  decodeListLenOf 2
  !a <- decode
  !b <- decode
  pure $! f a b

decodeProd3 :: (Serialise a, Serialise b, Serialise c) => (a -> b -> c -> r) -> Decoder s r
decodeProd3 f = do
  decodeListLenOf 3
  !a <- decode
  !b <- decode
  !c <- decode
  pure $! f a b c
