{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.BitSeq where

import Data.Bits (Bits (setBit, testBit, zeroBits), FiniteBits (finiteBitSize))
import Data.List qualified as List

data Bit = B0 | B1
  deriving (Eq, Ord, Read, Show)

newtype BitSeq = BitSeq {unBitSeq :: [Bit]}
  deriving stock (Eq, Ord)
  deriving newtype (Monoid, Read, Semigroup, Show)

length :: BitSeq -> Int
length = List.length . unBitSeq

repartitionPad :: (FiniteBits a, FiniteBits b) => Bit -> [a] -> [b]
repartitionPad padBit = toChunksPad padBit . fromChunks

repartitionUnpad :: (FiniteBits a, FiniteBits b) => [a] -> ([b], BitSeq)
repartitionUnpad = toChunksUnpad . fromChunks

fromList :: [Bit] -> BitSeq
fromList = BitSeq

toList :: BitSeq -> [Bit]
toList = unBitSeq

fromChunk :: FiniteBits a => a -> BitSeq
fromChunk a = fromList [a `getBit` i | i <- [0 .. finiteBitSize a - 1]]

fromChunks :: FiniteBits a => [a] -> BitSeq
fromChunks = fromList . concatMap (toList . fromChunk)

takeChunkPad :: forall a. FiniteBits a => Bit -> [Bit] -> (a, [Bit])
takeChunkPad padding bits = (chunk, rest)
  where
    w = finiteBitSize (zeroBits :: a)
    (prefix, rest) = splitAt w bits
    padded = prefix ++ replicate (w - List.length prefix) padding
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
    (prefix, rest) = splitAt w bits
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
toChunksPad :: forall a. FiniteBits a => Bit -> BitSeq -> [a]
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
toChunksUnpad :: forall a. FiniteBits a => BitSeq -> ([a], BitSeq)
toChunksUnpad (BitSeq bits) = go bits []
  where
    go [] acc = (reverse acc, BitSeq [])
    go bs acc =
      case takeChunkUnpad bs of
        Nothing -> (reverse acc, BitSeq bs) -- remaining bits < width
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
