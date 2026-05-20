{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeAbstractions #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Data.BitSeqSpec
  ( spec
  ) where

import Data.Bifunctor (Bifunctor (second))
import Data.Bit (Bit)
import Data.BitOrder (BitOrder)
import Data.BitSeq (BitSeq)
import Data.BitSeq qualified as BitSeq
import Data.Bits (FiniteBits (finiteBitSize))
import Data.Data (Proxy (Proxy))
import Data.Typeable (Typeable, typeRep)
import Data.Word (Word16, Word32, Word8)
import Data.Word2 (Word2)
import Data.Word3 (Word3)
import Data.Word5 (Word5)
import Test.Hspec (Spec, describe)
import Test.Hspec.QuickCheck (prop)
import Test.Hspec.QuickCheck.Classes (testLaws)
import Test.QuickCheck
  ( Arbitrary (arbitrary, shrink)
  , Property
  , arbitraryBoundedEnum
  , choose
  , elements
  , forAll
  , listOf
  , shrinkBoundedEnum
  , shrinkMap
  , (===)
  )
import Test.QuickCheck.Classes
  ( eqLaws
  , monoidLaws
  , ordLaws
  , semigroupLaws
  , semigroupMonoidLaws
  , showLaws
  , showReadLaws
  )

spec :: Spec
spec = do
  testLaws @BitSeq
    [ eqLaws
    , ordLaws
    , showLaws
    , showReadLaws
    , semigroupLaws
    , monoidLaws
    , semigroupMonoidLaws
    ]

  describe "fromList" $ do
    prop "prop_fromList_toList"
      \\\ prop_fromList_toList

  describe "takeChunkDeflate" $ do
    prop "prop_takeChunkDeflate_exact"
      \\\ prop_takeChunkDeflate_exact
    prop "prop_takeChunkDeflate_surplus"
      \\\ prop_takeChunkDeflate_surplus
    prop "prop_takeChunkDeflate_deficit"
      \\\ prop_takeChunkDeflate_deficit

  describe "takeChunkInflate" $ do
    prop "prop_takeChunkInflate_exact"
      \\\ prop_takeChunkInflate_exact
    prop "prop_takeChunkInflate_surplus"
      \\\ prop_takeChunkInflate_surplus
    prop "prop_takeChunkInflate_deficit"
      \\\ prop_takeChunkInflate_deficit

  describe "toChunksDeflate" $ do
    prop "prop_toChunksDeflate_roundTrip"
      \\\ prop_toChunksDeflate_roundTrip
    prop "prop_toChunksDeflate_remainder"
      \\\ prop_toChunksDeflate_remainder
    prop "prop_toChunksDeflate_exact"
      \\\ prop_toChunksDeflate_exact

  describe "toChunksInflate" $ do
    prop "prop_toChunksInflate_prefix"
      \\\ prop_toChunksInflate_prefix
    prop "prop_toChunksInflate_padding"
      \\\ prop_toChunksInflate_padding
    prop "prop_toChunksInflate_exact"
      \\\ prop_toChunksInflate_exact

(\\\) :: (a -> b) -> a -> b
(\\\) = ($)

--------------------------------------------------------------------------------
-- Utility types
--------------------------------------------------------------------------------

data ChunkType
  = forall a. (Arbitrary a, FiniteBits a, Show a, Typeable a) => ChunkType

instance Show ChunkType where
  show (ChunkType @a) = "ChunkType @" <> show (typeRep $ Proxy @a)

instance Arbitrary ChunkType where
  arbitrary =
    elements
      [ ChunkType @Word2
      , ChunkType @Word3
      , ChunkType @Word5
      , ChunkType @Word8
      , ChunkType @Word16
      , ChunkType @Word32
      ]

--------------------------------------------------------------------------------
-- Arbitrary instances
--------------------------------------------------------------------------------

instance Arbitrary Bit where
  arbitrary = arbitraryBoundedEnum
  shrink = shrinkBoundedEnum

instance Arbitrary BitOrder where
  arbitrary = arbitraryBoundedEnum
  shrink = shrinkBoundedEnum

instance Arbitrary BitSeq where
  arbitrary = BitSeq.fromList <$> arbitrary
  shrink = shrinkMap BitSeq.fromList BitSeq.toList

instance Arbitrary Word2 where
  arbitrary = arbitraryBoundedEnum
  shrink = shrinkBoundedEnum

instance Arbitrary Word3 where
  arbitrary = arbitraryBoundedEnum
  shrink = shrinkBoundedEnum

instance Arbitrary Word5 where
  arbitrary = arbitraryBoundedEnum
  shrink = shrinkBoundedEnum

--------------------------------------------------------------------------------
-- Properties: takeChunkDeflate
--------------------------------------------------------------------------------

-- A sequence exactly one chunk in length yields a chunk and no remaining bits.
prop_takeChunkDeflate_exact :: ChunkType -> BitOrder -> Property
prop_takeChunkDeflate_exact (ChunkType @chunkType) bitOrder =
  forAll (arbitrary @chunkType) $ \chunk ->
    BitSeq.takeChunkDeflate bitOrder (BitSeq.fromChunk bitOrder chunk)
      === Just (BitSeq.empty, chunk)

-- A sequence longer than one chunk yields a chunk and just the remaining bits.
prop_takeChunkDeflate_surplus :: ChunkType -> BitOrder -> BitSeq -> Property
prop_takeChunkDeflate_surplus (ChunkType @chunkType) bitOrder surplus =
  forAll (arbitrary @chunkType) $ \chunk ->
    BitSeq.takeChunkDeflate bitOrder (BitSeq.fromChunk bitOrder chunk <> surplus)
      === Just (surplus, chunk)

-- A sequence shorter than one chunk yields nothing.
prop_takeChunkDeflate_deficit :: ChunkType -> BitOrder -> Property
prop_takeChunkDeflate_deficit (ChunkType @chunkType) bitOrder =
  forAll (arbitrary @chunkType) $ \chunk ->
    forAll (choose (0, width @chunkType - 1)) $ \bitsToTake ->
      BitSeq.takeChunkDeflate @chunkType
        bitOrder
        (BitSeq.take bitsToTake (BitSeq.fromChunk bitOrder chunk))
        === Nothing

--------------------------------------------------------------------------------
-- Properties: takeChunkInflate
--------------------------------------------------------------------------------

-- A sequence exactly one chunk in length yields a chunk with no padding.
prop_takeChunkInflate_exact :: ChunkType -> BitOrder -> Bit -> Property
prop_takeChunkInflate_exact (ChunkType @chunkType) bitOrder padding =
  forAll (arbitrary @chunkType) $ \chunk ->
    BitSeq.takeChunkInflate bitOrder padding (BitSeq.fromChunk bitOrder chunk)
      === (BitSeq.empty, chunk)

-- A sequence longer than one chunk yields a chunk and just the remaining bits.
prop_takeChunkInflate_surplus
  :: ChunkType -> BitOrder -> Bit -> BitSeq -> Property
prop_takeChunkInflate_surplus (ChunkType @chunkType) bitOrder padding surplus =
  forAll (arbitrary @chunkType) $ \chunk ->
    BitSeq.takeChunkInflate
      bitOrder
      padding
      (BitSeq.fromChunk bitOrder chunk <> surplus)
      === (surplus, chunk)

-- A sequence shorter than one chunk yields a padded chunk.
prop_takeChunkInflate_deficit :: ChunkType -> BitOrder -> Bit -> Property
prop_takeChunkInflate_deficit (ChunkType @chunkType) bitOrder paddingBit =
  forAll (arbitrary @chunkType) $ \chunk ->
    forAll (choose (1, width @chunkType)) $ \paddingLength -> do
      let prefixLength = width @chunkType - paddingLength
      let prefix = BitSeq.take prefixLength (BitSeq.fromChunk bitOrder chunk)
      let padding = BitSeq.replicate paddingLength paddingBit
      let result = BitSeq.takeChunkInflate @chunkType bitOrder paddingBit prefix
      second (BitSeq.fromChunk bitOrder) result
        === (BitSeq.empty, prefix <> padding)

--------------------------------------------------------------------------------
-- Properties: toChunksDeflate
--------------------------------------------------------------------------------

-- The chunks and remainder reconstruct the original sequence.
prop_toChunksDeflate_roundTrip :: ChunkType -> BitOrder -> BitSeq -> Property
prop_toChunksDeflate_roundTrip (ChunkType @chunkType) bitOrder bitSeq = do
  let (remainder, chunks) = BitSeq.toChunksDeflate @chunkType bitOrder bitSeq
  BitSeq.fromChunks bitOrder chunks <> remainder === bitSeq

-- The remainder is minimal: the sequence cannot yield another chunk.
prop_toChunksDeflate_remainder :: ChunkType -> BitOrder -> BitSeq -> Property
prop_toChunksDeflate_remainder (ChunkType @chunkType) bitOrder bitSeq = do
  let (remainder, _) = BitSeq.toChunksDeflate @chunkType bitOrder bitSeq
  BitSeq.length remainder `compare` width @chunkType === LT

-- When the chunk width exactly divides the sequence, the remainder is empty.
prop_toChunksDeflate_exact :: ChunkType -> BitOrder -> Property
prop_toChunksDeflate_exact (ChunkType @chunkType) bitOrder =
  forAll (listOf (arbitrary @chunkType)) $ \chunks ->
    BitSeq.toChunksDeflate @chunkType
      bitOrder
      (BitSeq.fromChunks bitOrder chunks)
      === (BitSeq.empty, chunks)

--------------------------------------------------------------------------------
-- Properties: toChunksInflate
--------------------------------------------------------------------------------

-- The original sequence is a prefix of the coalesced chunks.
prop_toChunksInflate_prefix
  :: ChunkType -> BitOrder -> Bit -> BitSeq -> Property
prop_toChunksInflate_prefix (ChunkType @chunkType) bitOrder paddingBit bitSeq =
  do
    let chunks = BitSeq.toChunksInflate @chunkType bitOrder paddingBit bitSeq
    BitSeq.take (BitSeq.length bitSeq) (BitSeq.fromChunks bitOrder chunks)
      === bitSeq

-- The padding bits extend the remainder exactly to a chunk boundary.
prop_toChunksInflate_padding
  :: ChunkType -> BitOrder -> Bit -> BitSeq -> Property
prop_toChunksInflate_padding (ChunkType @chunkType) bitOrder paddingBit bitSeq =
  do
    let chunks = BitSeq.toChunksInflate @chunkType bitOrder paddingBit bitSeq
    let paddingLength = negate (BitSeq.length bitSeq) `mod` width @chunkType
    BitSeq.drop (BitSeq.length bitSeq) (BitSeq.fromChunks bitOrder chunks)
      === BitSeq.replicate paddingLength paddingBit

-- When the chunk width exactly divides the sequence, there is no padding.
prop_toChunksInflate_exact :: ChunkType -> BitOrder -> Bit -> Property
prop_toChunksInflate_exact (ChunkType @chunkType) bitOrder paddingBit =
  forAll (listOf (arbitrary @chunkType)) $ \chunks -> do
    BitSeq.toChunksInflate @chunkType
      bitOrder
      paddingBit
      (BitSeq.fromChunks bitOrder chunks)
      === chunks

--------------------------------------------------------------------------------
-- Properties: fromList
--------------------------------------------------------------------------------

prop_fromList_toList :: [Bit] -> Property
prop_fromList_toList bits =
  BitSeq.toList (BitSeq.fromList bits) === bits

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

_compareWidth :: forall a b. FiniteBits a => FiniteBits b => Ordering
_compareWidth = compare (width @a) (width @b)

width :: forall a. FiniteBits a => Int
width = finiteBitSize @a undefined
