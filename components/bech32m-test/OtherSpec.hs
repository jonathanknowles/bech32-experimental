{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module OtherSpec (spec) where

import Codec.Bech32.Prefix (Prefix)
import Codec.Bech32.Prefix qualified as Prefix
import Codec.Bech32.Prefix.Char (PrefixChar)
import Codec.Bech32.Suffix.Char (SuffixChar)
import Codec.Bech32.Suffix.Char qualified as SuffixChar
import Codec.Bech32.Suffix.Payload (Payload)
import Codec.Bech32.Suffix.Payload qualified as Payload
import Data.Bit (Bit)
import Data.List.NonEmpty (NonEmpty)
import Data.Word2 (Word2)
import Data.Word3 (Word3)
import Data.Word5 (Word5)
import Test.Hspec (Spec, describe, it)
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
  , numLaws
  , ordLaws
  , semigroupLaws
  , showLaws
  , showReadLaws
  )

spec :: Spec
spec = do
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
    testLaws @Word2
      [ bitsLaws
      , boundedEnumLaws
      , eqLaws
      , ixLaws
      , numLaws
      , ordLaws
      , showLaws
      , showReadLaws
      ]
    testLaws @Word3
      [ bitsLaws
      , boundedEnumLaws
      , eqLaws
      , ixLaws
      , numLaws
      , ordLaws
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
