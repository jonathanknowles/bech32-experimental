{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeAbstractions #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Data.BitSeqSpec
  ( spec
  ) where

import Data.Bit (Bit)
import Data.BitSeq qualified as BitSeq
import Data.Bits (FiniteBits)
import Data.Word (Word16, Word32, Word8)
import Data.Word2 (Word2)
import Data.Word3 (Word3)
import Data.Word5 (Word5)
import Test.Hspec (Spec, describe, it)
import Test.QuickCheck
  ( Arbitrary (arbitrary, shrink)
  , Property
  , Testable (property)
  , arbitraryBoundedEnum
  , elements
  , forAll
  , shrinkBoundedEnum
  , (===)
  )

spec :: Spec
spec = describe "Properties" $ do
  it "prop_fromChunk_takeChunkDeflate" $
    property
      prop_fromChunk_takeChunkDeflate
  it "prop_fromChunk_takeChunkInflate" $
    property
      prop_fromChunk_takeChunkInflate
  it "prop_fromList_toList" $
    property
      prop_fromList_toList

--------------------------------------------------------------------------------
-- Utility types
--------------------------------------------------------------------------------

data ChunkType = forall a. (Arbitrary a, FiniteBits a, Show a) => ChunkType

instance Show ChunkType where
  show (ChunkType @_) = "ChunkType"

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
-- Properties
--------------------------------------------------------------------------------

prop_fromChunk_takeChunkDeflate :: ChunkType -> Property
prop_fromChunk_takeChunkDeflate (ChunkType @a) =
  forAll (arbitrary @a) $ \chunk ->
    BitSeq.takeChunkDeflate (BitSeq.fromChunk chunk)
      === Just (chunk, BitSeq.empty)

prop_fromChunk_takeChunkInflate :: ChunkType -> Bit -> Property
prop_fromChunk_takeChunkInflate (ChunkType @a) paddingBit =
  forAll (arbitrary @a) $ \chunk ->
    BitSeq.takeChunkInflate paddingBit (BitSeq.fromChunk chunk)
      === (chunk, BitSeq.empty)

prop_fromList_toList :: [Bit] -> Property
prop_fromList_toList bits =
  BitSeq.toList (BitSeq.fromList bits) === bits
