{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeAbstractions #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Data.BitSeqSpec
  ( spec
  ) where

import Data.Bit (Bit)
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
      <+> prop_fromList_toList
  describe "takeChunkDeflate" $ do
    prop "prop_takeChunkDeflate_exact"
      <+> prop_takeChunkDeflate_exact
    prop "prop_takeChunkDeflate_surplus"
      <+> prop_takeChunkDeflate_surplus
    prop "prop_takeChunkDeflate_deficit"
      <+> prop_takeChunkDeflate_deficit
  describe "takeChunkInflate" $ do
    prop "prop_takeChunkInflate_exact"
      <+> prop_takeChunkInflate_exact
    prop "prop_takeChunkInflate_surplus"
      <+> prop_takeChunkInflate_surplus
    prop "prop_takeChunkInflate_deficit"
      <+> prop_takeChunkInflate_deficit

(<+>) :: (a -> b) -> a -> b
(<+>) = ($)

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

prop_takeChunkDeflate_exact :: ChunkType -> Property
prop_takeChunkDeflate_exact (ChunkType @chunkType) =
  forAll (arbitrary @chunkType) $ \chunk ->
    BitSeq.takeChunkDeflate (BitSeq.fromChunk chunk)
      === Just (BitSeq.empty, chunk)

prop_takeChunkDeflate_surplus :: ChunkType -> BitSeq -> Property
prop_takeChunkDeflate_surplus (ChunkType @chunkType) surplus =
  forAll (arbitrary @chunkType) $ \chunk ->
    BitSeq.takeChunkDeflate (BitSeq.fromChunk chunk <> surplus)
      === Just (surplus, chunk)

prop_takeChunkDeflate_deficit :: ChunkType -> Property
prop_takeChunkDeflate_deficit (ChunkType @chunkType) =
  forAll (arbitrary @chunkType) $ \chunk ->
    forAll (choose (0, width @chunkType - 1)) $ \bitsToTake ->
      BitSeq.takeChunkDeflate @chunkType
        (BitSeq.take bitsToTake (BitSeq.fromChunk chunk))
        === Nothing

--------------------------------------------------------------------------------
-- Properties: takeChunkInflate
--------------------------------------------------------------------------------

prop_takeChunkInflate_exact :: ChunkType -> Bit -> Property
prop_takeChunkInflate_exact (ChunkType @chunkType) padding =
  forAll (arbitrary @chunkType) $ \chunk ->
    BitSeq.takeChunkInflate padding (BitSeq.fromChunk chunk)
      === (BitSeq.empty, chunk)

prop_takeChunkInflate_surplus :: ChunkType -> Bit -> BitSeq -> Property
prop_takeChunkInflate_surplus (ChunkType @chunkType) padding surplus =
  forAll (arbitrary @chunkType) $ \chunk ->
    BitSeq.takeChunkInflate padding (BitSeq.fromChunk chunk <> surplus)
      === (surplus, chunk)

prop_takeChunkInflate_deficit :: ChunkType -> Bit -> Property
prop_takeChunkInflate_deficit (ChunkType @chunkType) padding =
  forAll (arbitrary @chunkType) $ \chunk ->
    forAll (choose (0, width @chunkType - 1)) $ \bitsToTake ->
      BitSeq.takeChunkInflate
        @chunkType
        padding
        (BitSeq.take bitsToTake (BitSeq.fromChunk chunk))
        === (BitSeq.empty, undefined)

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
