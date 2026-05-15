{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Main (main) where

import Codec.Bech32.Prefix (Prefix)
import Codec.Bech32.Prefix qualified as Prefix
import Codec.Bech32.Prefix.Char (PrefixChar)
import Codec.Bech32.Prefix.Char qualified as PrefixChar
import Codec.Bech32.Suffix.Char (SuffixChar)
import Codec.Bech32.Suffix.Char qualified as SuffixChar
import Codec.Bech32.Suffix.Payload (Payload)
import Codec.Bech32.Suffix.Payload qualified as Payload
import Data.Bit (Bit)
import Data.List.NonEmpty (NonEmpty)
import Data.Word (Word8)
import Data.Word5 (Word5)
import Test.Hspec (describe, hspec, it)
import Test.Hspec.QuickCheck.Classes (testLaws)
import Test.QuickCheck
  ( Arbitrary (arbitrary, shrink)
  , Property
  , Testable (property)
  , arbitraryBoundedEnum
  , shrinkBoundedEnum
  , shrinkMap
  , (===)
  )
import Test.QuickCheck.Arbitrary ()
import Test.QuickCheck.Classes
  ( bitsLaws
  , boundedEnumLaws
  , eqLaws
  , ixLaws
  , monoidLaws
  , numLaws
  , ordLaws
  , semigroupLaws
  , semigroupMonoidLaws
  , showLaws
  , showReadLaws
  )

main :: IO ()
main = hspec $ do
  describe "Class laws" $ do
    testLaws @Bit
      [ bitsLaws
      , boundedEnumLaws
      , eqLaws
      , ixLaws
      , numLaws
      , ordLaws
      , showLaws
      , showReadLaws
      ]
    testLaws @SuffixChar
      [ boundedEnumLaws
      , eqLaws
      , ixLaws
      , ordLaws
      , showLaws
      , showReadLaws
      ]
    testLaws @Payload
      [ eqLaws
      , monoidLaws
      , ordLaws
      , semigroupLaws
      , semigroupMonoidLaws
      , showLaws
      , showReadLaws
      ]
    testLaws @PrefixChar
      [ boundedEnumLaws
      , eqLaws
      , ixLaws
      , ordLaws
      , showLaws
      , showReadLaws
      ]
    testLaws @Prefix
      [ eqLaws
      , ordLaws
      , semigroupLaws
      , showLaws
      , showReadLaws
      ]
    testLaws @Word5
      [ bitsLaws
      , boundedEnumLaws
      , eqLaws
      , ixLaws
      , numLaws
      , ordLaws
      , showLaws
      , showReadLaws
      ]
  describe "Properties" $ do
    describe "SuffixChar" $ do
      it "prop_SuffixChar_fromWord5_toWord5" $
        property
          prop_SuffixChar_fromWord5_toWord5
      it "prop_SuffixChar_toWord5_fromWord5" $
        property
          prop_SuffixChar_toWord5_fromWord5
      it "prop_SuffixChar_toChar_fromCharMaybe" $
        property
          prop_SuffixChar_toChar_fromCharMaybe
    describe "Payload" $ do
      it "prop_Payload_append_fromWord5List" $
        property
          prop_Payload_append_fromWord5List
      it "prop_Payload_append_toWord5List" $
        property
          prop_Payload_append_toWord5List
      it "prop_Payload_toText_fromText" $
        property
          prop_Payload_toText_fromText
      it "prop_Payload_toWord5List_fromWord5List" $
        property
          prop_Payload_toWord5List_fromWord5List
      it "prop_Payload_fromWord5List_toWord5List" $
        property
          prop_Payload_fromWord5List_toWord5List
      it "prop_Payload_fromWord8List_toWord8List" $
        property
          prop_Payload_fromWord8List_toWord8List
    describe "PrefixChar" $ do
      it "prop_PrefixChar_toChar_fromCharMaybe" $
        property
          prop_PrefixChar_toChar_fromCharMaybe
    describe "Prefix" $ do
      it "prop_Prefix_append_fromList" $
        property
          prop_Prefix_append_fromList
      it "prop_Prefix_append_toList" $
        property
          prop_Prefix_append_toList
      it "prop_Prefix_toText_fromText" $
        property
          prop_Prefix_toText_fromText
      it "prop_Prefix_fromList_toList" $
        property
          prop_Prefix_fromList_toList
      it "prop_Prefix_toList_fromList" $
        property
          prop_Prefix_toList_fromList

--------------------------------------------------------------------------------
-- Arbitrary instances
--------------------------------------------------------------------------------

instance Arbitrary Bit where
  arbitrary = arbitraryBoundedEnum
  shrink = shrinkBoundedEnum

instance Arbitrary SuffixChar where
  arbitrary = arbitraryBoundedEnum
  shrink = shrinkBoundedEnum

instance Arbitrary Payload where
  arbitrary = Payload.fromWord5List <$> arbitrary
  shrink = shrinkMap Payload.fromWord5List Payload.toWord5List

instance Arbitrary PrefixChar where
  arbitrary = arbitraryBoundedEnum
  shrink = shrinkBoundedEnum

instance Arbitrary Prefix where
  arbitrary = Prefix.fromList <$> arbitrary
  shrink = shrinkMap Prefix.fromList Prefix.toList

instance Arbitrary Word5 where
  arbitrary = arbitraryBoundedEnum
  shrink = shrinkBoundedEnum

--------------------------------------------------------------------------------
-- Properties for SuffixChar
--------------------------------------------------------------------------------

prop_SuffixChar_fromWord5_toWord5 :: Word5 -> Property
prop_SuffixChar_fromWord5_toWord5 w =
  SuffixChar.toWord5 (SuffixChar.fromWord5 w) === w

prop_SuffixChar_toWord5_fromWord5 :: SuffixChar -> Property
prop_SuffixChar_toWord5_fromWord5 c =
  SuffixChar.fromWord5 (SuffixChar.toWord5 c) === c

prop_SuffixChar_toChar_fromCharMaybe :: SuffixChar -> Property
prop_SuffixChar_toChar_fromCharMaybe c =
  SuffixChar.fromCharMaybe (SuffixChar.toChar c) === Just c

--------------------------------------------------------------------------------
-- Properties for Payload
--------------------------------------------------------------------------------

prop_Payload_append_fromWord5List :: [Word5] -> [Word5] -> Property
prop_Payload_append_fromWord5List ps qs =
  Payload.fromWord5List (ps <> qs)
    === (Payload.fromWord5List ps <> Payload.fromWord5List qs)

prop_Payload_append_toWord5List :: Payload -> Payload -> Property
prop_Payload_append_toWord5List p q =
  Payload.toWord5List (p <> q)
    === (Payload.toWord5List p <> Payload.toWord5List q)

prop_Payload_toText_fromText :: Payload -> Property
prop_Payload_toText_fromText d =
  Payload.fromText (Payload.toText d) === Right d

prop_Payload_toWord5List_fromWord5List :: Payload -> Property
prop_Payload_toWord5List_fromWord5List d =
  Payload.fromWord5List (Payload.toWord5List d) === d

prop_Payload_fromWord5List_toWord5List :: [Word5] -> Property
prop_Payload_fromWord5List_toWord5List ws =
  Payload.toWord5List (Payload.fromWord5List ws) === ws

prop_Payload_fromWord8List_toWord8List :: [Word8] -> Property
prop_Payload_fromWord8List_toWord8List ws =
  Payload.toWord8List (Payload.fromWord8List ws) === Just ws

--------------------------------------------------------------------------------
-- Properties for PrefixChar
--------------------------------------------------------------------------------

prop_PrefixChar_toChar_fromCharMaybe :: PrefixChar -> Property
prop_PrefixChar_toChar_fromCharMaybe c =
  PrefixChar.fromCharMaybe (PrefixChar.toChar c) === Just c

--------------------------------------------------------------------------------
-- Properties for Prefix
--------------------------------------------------------------------------------

prop_Prefix_append_fromList
  :: NonEmpty PrefixChar
  -> NonEmpty PrefixChar
  -> Property
prop_Prefix_append_fromList ps qs =
  Prefix.fromList (ps <> qs)
    === (Prefix.fromList ps <> Prefix.fromList qs)

prop_Prefix_append_toList
  :: Prefix
  -> Prefix
  -> Property
prop_Prefix_append_toList p q =
  Prefix.toList (p <> q)
    === (Prefix.toList p <> Prefix.toList q)

prop_Prefix_toText_fromText :: Prefix -> Property
prop_Prefix_toText_fromText d =
  Prefix.fromText (Prefix.toText d) === Right d

prop_Prefix_toList_fromList :: Prefix -> Property
prop_Prefix_toList_fromList d =
  Prefix.fromList (Prefix.toList d) === d

prop_Prefix_fromList_toList :: NonEmpty PrefixChar -> Property
prop_Prefix_fromList_toList ws =
  Prefix.toList (Prefix.fromList ws) === ws
