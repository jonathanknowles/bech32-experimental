{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.BitSeq
  ( BitSeq
  , ChunkBitOrder (..)
  , all
  , any
  , drop
  , empty
  , fromChunk
  , fromChunks
  , fromList
  , length
  , null
  , repeat
  , replicate
  , singleton
  , take
  , takeChunkDeflate
  , takeChunkInflate
  , toChunksDeflate
  , toChunksInflate
  , toList
  )
where

import Data.Bit (Bit (B0, B1))
import Data.Bits (Bits (setBit, testBit, zeroBits), FiniteBits (finiteBitSize))
import Data.List qualified as List
import Data.Semigroup.Cancellative (LeftReductive, RightReductive)
import Prelude hiding (all, any, drop, length, null, repeat, replicate, take)
import Prelude qualified

newtype BitSeq = BitSeq {unBitSeq :: [Bit]}
  deriving stock (Eq, Ord)
  deriving newtype
    ( LeftReductive
    , Monoid
    , Read
    , RightReductive
    , Semigroup
    , Show
    )

empty :: BitSeq
empty = BitSeq []

null :: BitSeq -> Bool
null (BitSeq s) = Prelude.null s

singleton :: Bit -> BitSeq
singleton b = BitSeq [b]

repeat :: Bit -> BitSeq
repeat b = BitSeq (List.repeat b)

replicate :: Int -> Bit -> BitSeq
replicate n b = BitSeq (Prelude.replicate n b)

drop :: Int -> BitSeq -> BitSeq
drop n (BitSeq s) = BitSeq (Prelude.drop n s)

take :: Int -> BitSeq -> BitSeq
take n (BitSeq s) = BitSeq (Prelude.take n s)

all :: (Bit -> Bool) -> BitSeq -> Bool
all f = List.all f . toList

any :: (Bit -> Bool) -> BitSeq -> Bool
any f = List.any f . toList

length :: BitSeq -> Int
length = List.length . unBitSeq

fromList :: [Bit] -> BitSeq
fromList = BitSeq

toList :: BitSeq -> [Bit]
toList = unBitSeq

data ChunkBitOrder
  = FromLSBToMSB
  | FromMSBToLSB

fromChunk :: FiniteBits a => ChunkBitOrder -> a -> BitSeq
fromChunk bitOrder a = fromList [a `getBit` i | i <- indices]
  where
    indices = case bitOrder of
      FromLSBToMSB -> [lsb, lsb + 1 .. msb]
      FromMSBToLSB -> [msb, msb - 1 .. lsb]
    lsb = 0
    msb = finiteBitSize a - 1

fromChunks :: FiniteBits a => ChunkBitOrder -> [a] -> BitSeq
fromChunks bitOrder = fromList . concatMap (toList . fromChunk bitOrder)

takeChunkDeflate :: forall a. FiniteBits a => BitSeq -> Maybe (BitSeq, a)
takeChunkDeflate (BitSeq bits)
  | List.length prefix < w = Nothing
  | otherwise = Just (BitSeq rest, chunk)
  where
    w = finiteBitSize (zeroBits :: a)
    (prefix, rest) = List.splitAt w bits
    chunk =
      List.foldl'
        (\acc (i, b) -> setBitFrom b i acc)
        zeroBits
        (zip [0 ..] prefix)

takeChunkInflate :: forall a. FiniteBits a => Bit -> BitSeq -> (BitSeq, a)
takeChunkInflate padding (BitSeq bits) = (BitSeq rest, chunk)
  where
    w = finiteBitSize (zeroBits :: a)
    (prefix, rest) = List.splitAt w bits
    padded = prefix ++ List.replicate (w - List.length prefix) padding
    chunk =
      List.foldl'
        (\acc (i, b) -> setBitFrom b i acc)
        zeroBits
        (zip [0 ..] padded)

toChunksDeflate :: forall a. FiniteBits a => BitSeq -> (BitSeq, [a])
toChunksDeflate (BitSeq bits) = go bits []
  where
    go [] acc = (BitSeq [], reverse acc)
    go bs acc =
      case takeChunkDeflate (BitSeq bs) of
        Just (BitSeq rest, chunk) -> go rest (chunk : acc)
        Nothing -> (BitSeq bs, reverse acc)

toChunksInflate :: forall a. FiniteBits a => Bit -> BitSeq -> [a]
toChunksInflate padding (BitSeq bits) = go bits
  where
    go [] = []
    go bs =
      let (BitSeq rest, chunk) = takeChunkInflate padding (BitSeq bs)
      in chunk : go rest

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
