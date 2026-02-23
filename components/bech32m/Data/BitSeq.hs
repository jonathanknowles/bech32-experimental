{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.BitSeq
  ( Bit (..)
  , BitSeq
  , all
  , any
  , length
  , repeat
  , singleton
  , fromList
  , fromChunks
  , toList
  , toChunksDeflate
  , toChunksInflate
  , repartitionDeflate
  , repartitionInflate
  )
where

import Data.Bits (Bits (setBit, testBit, zeroBits), FiniteBits (finiteBitSize))
import Data.Ix (Ix)
import Data.List qualified as List
import GHC.Generics (Generic)
import Prelude hiding (all, any, length, repeat, take)

-- TODO: Move to separate module and add Num + Bits instances.
data Bit = B0 | B1
  deriving (Bounded, Enum, Eq, Generic, Ix, Ord, Read, Show)

newtype BitSeq = BitSeq {unBitSeq :: [Bit]}
  deriving stock (Eq, Ord)
  deriving newtype (Monoid, Read, Semigroup, Show)

singleton :: Bit -> BitSeq
singleton b = BitSeq [b]

repeat :: Bit -> BitSeq
repeat b = BitSeq (List.repeat b)

all :: (Bit -> Bool) -> BitSeq -> Bool
all f = List.all f . toList

any :: (Bit -> Bool) -> BitSeq -> Bool
any f = List.any f . toList

length :: BitSeq -> Int
length = List.length . unBitSeq

repartitionInflate :: (FiniteBits a, FiniteBits b) => Bit -> [a] -> [b]
repartitionInflate padding = toChunksInflate padding . fromChunks

repartitionDeflate :: (FiniteBits a, FiniteBits b) => [a] -> (BitSeq, [b])
repartitionDeflate = toChunksDeflate . fromChunks

fromList :: [Bit] -> BitSeq
fromList = BitSeq

toList :: BitSeq -> [Bit]
toList = unBitSeq

fromChunk :: FiniteBits a => a -> BitSeq
fromChunk a = fromList [a `getBit` i | i <- [0 .. finiteBitSize a - 1]]

fromChunks :: FiniteBits a => [a] -> BitSeq
fromChunks = fromList . concatMap (toList . fromChunk)

takeChunkInflate :: forall a. FiniteBits a => Bit -> BitSeq -> (a, BitSeq)
takeChunkInflate padding (BitSeq bits) = (chunk, BitSeq rest)
  where
    w = finiteBitSize (zeroBits :: a)
    (prefix, rest) = List.splitAt w bits
    padded = prefix ++ List.replicate (w - List.length prefix) padding
    chunk =
      List.foldl'
        (\acc (i, b) -> setBitFrom b i acc)
        zeroBits
        (zip [0 ..] padded)

takeChunkDeflate :: forall a. FiniteBits a => BitSeq -> Maybe (a, BitSeq)
takeChunkDeflate (BitSeq bits)
  | List.length prefix < w = Nothing
  | otherwise = Just (chunk, BitSeq rest)
  where
    w = finiteBitSize (zeroBits :: a)
    (prefix, rest) = List.splitAt w bits
    chunk =
      List.foldl'
        (\acc (i, b) -> setBitFrom b i acc)
        zeroBits
        (zip [0 ..] prefix)

toChunksInflate :: forall a. FiniteBits a => Bit -> BitSeq -> [a]
toChunksInflate padding (BitSeq bits) = go bits
  where
    go [] = []
    go bs =
      let (chunk, BitSeq rest) = takeChunkInflate padding (BitSeq bs)
      in chunk : go rest

toChunksDeflate :: forall a. FiniteBits a => BitSeq -> (BitSeq, [a])
toChunksDeflate (BitSeq bits) = go bits []
  where
    go [] acc = (BitSeq [], reverse acc)
    go bs acc =
      case takeChunkDeflate (BitSeq bs) of
        Just (chunk, BitSeq rest) -> go rest (chunk : acc)
        Nothing -> (BitSeq bs, reverse acc)

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

getBit :: Bits a => a -> Int -> Bit
getBit a i = case testBit a i of
  False -> B0
  True -> B1

setBitFrom :: Bits a => Bit -> Int -> a -> a
setBitFrom B0 _ acc = acc
setBitFrom B1 i acc = setBit acc i
