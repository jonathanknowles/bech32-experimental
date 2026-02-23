{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.BitSeq where

import Data.Bits (Bits (setBit, testBit, zeroBits), FiniteBits (finiteBitSize))
import Data.List qualified as List
import Prelude hiding (take, repeat)
import Data.Ix (Ix)
import GHC.Generics (Generic)

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

-- the padding should be non-empty...
repartitionPad :: (FiniteBits a, FiniteBits b) => BitSeq -> [a] -> [b]
repartitionPad padBits = toChunksPad padBits . fromChunks

repartitionUnpad :: (FiniteBits a, FiniteBits b) => [a] -> (BitSeq, [b])
repartitionUnpad = toChunksUnpad . fromChunks

fromList :: [Bit] -> BitSeq
fromList = BitSeq

toList :: BitSeq -> [Bit]
toList = unBitSeq

fromChunk :: FiniteBits a => a -> BitSeq
fromChunk a = fromList [a `getBit` i | i <- [0 .. finiteBitSize a - 1]]

fromChunks :: FiniteBits a => [a] -> BitSeq
fromChunks = fromList . concatMap (toList . fromChunk)

takeChunkPad :: forall a. FiniteBits a => BitSeq -> [Bit] -> (a, [Bit])
takeChunkPad (BitSeq padding) bits = (chunk, rest)
  where
    w = finiteBitSize (zeroBits :: a)
    (prefix, rest) = List.splitAt w bits
    padded = prefix ++ List.take (w - List.length prefix) (List.cycle padding)
    chunk =
      List.foldl'
        (\acc (i, b) -> setBitFrom b i acc)
        zeroBits
        (zip [0 ..] padded)

takeChunkUnpad :: forall a. FiniteBits a => [Bit] -> Maybe (a, [Bit])
takeChunkUnpad bits
  | List.length prefix < w = Nothing
  | otherwise = Just (chunk, rest)
  where
    w = finiteBitSize (zeroBits :: a)
    (prefix, rest) = List.splitAt w bits
    chunk =
      List.foldl'
        (\acc (i, b) -> setBitFrom b i acc)
        zeroBits
        (zip [0 ..] prefix)

-- | Converts the given 'BitSeq' to a list of finitely-sized chunks.
--
-- The total number of bits in the output is guaranteed to be greater than or
-- equal to the number of bits in the input, with the excess bits provided by
-- the given pad 'Bit'.
--
-- The padding length is guaranteed to be less than the width of a chunk.
toChunksPad :: forall a. FiniteBits a => BitSeq -> BitSeq -> [a]
toChunksPad padding (BitSeq bits) = go bits
  where
    go [] = []
    go bs =
      let (chunk, rest) = takeChunkPad padding bs
      in chunk : go rest

-- | Converts the given 'BitSeq' to a list of finitely-sized chunks.
--
-- The total number of bits in the output is guaranteed to be less than or
-- equal to the number of bits in the input.
--
-- The result includes a 'BitSeq' of the remaining bits, whose length is
-- guaranteed to be less than the width of a chunk.
toChunksUnpad :: forall a. FiniteBits a => BitSeq -> (BitSeq, [a])
toChunksUnpad (BitSeq bits) = go bits []
  where
    go [] acc = (BitSeq [], reverse acc)
    go bs acc =
      case takeChunkUnpad bs of
        Nothing -> (BitSeq bs, reverse acc) -- remaining bits < width
        Just (chunk, rest) -> go rest (chunk : acc)

drop :: Int -> BitSeq -> BitSeq
drop n = BitSeq . List.drop n . unBitSeq

take :: Int -> BitSeq -> BitSeq
take n = BitSeq . List.take n . unBitSeq

cons :: FiniteBits a => a -> BitSeq -> BitSeq
cons a bs = fromChunk a <> bs

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
