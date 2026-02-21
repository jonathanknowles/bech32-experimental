{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Main (main) where

import Codec.Bech32.DataChar (DataChar)
import Codec.Bech32.DataChar qualified as DataChar
import Codec.Bech32.DataPart (DataPart)
import Codec.Bech32.DataPart qualified as DataPart
import Codec.Bech32.HumanReadableChar (HumanReadableChar)
import Codec.Bech32.HumanReadableChar qualified as HumanReadableChar
import Codec.Bech32.HumanReadablePart (HumanReadablePart)
import Codec.Bech32.HumanReadablePart qualified as HumanReadablePart
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as List (NonEmpty)
import Data.Word5 (Word5)
import Test.Hspec (describe, hspec, it)
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
  ( boundedEnumLaws
  , eqLaws
  , ixLaws
  , monoidLaws
  , numLaws
  , ordLaws
  , semigroupLaws
  , semigroupMonoidLaws
  , showLaws
  , showReadLaws, bitsLaws
  )
import Test.QuickCheck.Classes.Hspec (testLawsMany)

main :: IO ()
main = hspec $ do
  describe "Class laws" $ do
    testLawsMany @DataChar
      [ boundedEnumLaws
      , eqLaws
      , ixLaws
      , ordLaws
      , showLaws
      , showReadLaws
      ]
    testLawsMany @DataPart
      [ eqLaws
      , monoidLaws
      , ordLaws
      , semigroupLaws
      , semigroupMonoidLaws
      , showLaws
      , showReadLaws
      ]
    testLawsMany @HumanReadableChar
      [ boundedEnumLaws
      , eqLaws
      , ixLaws
      , ordLaws
      , showLaws
      , showReadLaws
      ]
    testLawsMany @HumanReadablePart
      [ eqLaws
      , ordLaws
      , semigroupLaws
      , showLaws
      , showReadLaws
      ]
    testLawsMany @Word5
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
    describe "DataChar" $ do
      it "prop_DataChar_fromWord5_toWord5" $
        property
          prop_DataChar_fromWord5_toWord5
      it "prop_DataChar_toWord5_fromWord5" $
        property
          prop_DataChar_toWord5_fromWord5
      it "prop_DataChar_toChar_fromCharMaybe" $
        property
          prop_DataChar_toChar_fromCharMaybe
    describe "DataPart" $ do
      it "prop_DataPart_append_fromWordList" $
        property
          prop_DataPart_append_fromWordList
      it "prop_DataPart_append_toWordList" $
        property
          prop_DataPart_append_toWordList
      it "prop_DataPart_toText_fromText" $
        property
          prop_DataPart_toText_fromText
      it "prop_DataPart_toWordList_fromWordList" $
        property
          prop_DataPart_toWordList_fromWordList
      it "prop_DataPart_fromWordList_toWordList" $
        property
          prop_DataPart_fromWordList_toWordList
    describe "HumanReadableChar" $ do
      it "prop_HumanReadableChar_toChar_fromCharMaybe" $
        property
          prop_HumanReadableChar_toChar_fromCharMaybe
    describe "HumanReadablePart" $ do
      it "prop_HumanReadablePart_append_fromList" $
        property
          prop_HumanReadablePart_append_fromList
      it "prop_HumanReadablePart_append_toList" $
        property
          prop_HumanReadablePart_append_toList
      it "prop_HumanReadablePart_toText_fromText" $
        property
          prop_HumanReadablePart_toText_fromText
      it "prop_HumanReadablePart_fromList_toList" $
        property
          prop_HumanReadablePart_fromList_toList
      it "prop_HumanReadablePart_toList_fromList" $
        property
          prop_HumanReadablePart_toList_fromList

--------------------------------------------------------------------------------
-- Arbitrary instances
--------------------------------------------------------------------------------

instance Arbitrary DataChar where
  arbitrary = arbitraryBoundedEnum
  shrink = shrinkBoundedEnum

instance Arbitrary DataPart where
  arbitrary = DataPart.fromWordList <$> arbitrary
  shrink = shrinkMap DataPart.fromWordList DataPart.toWordList

instance Arbitrary HumanReadableChar where
  arbitrary = arbitraryBoundedEnum
  shrink = shrinkBoundedEnum

instance Arbitrary HumanReadablePart where
  arbitrary = HumanReadablePart.fromList <$> arbitrary
  shrink = shrinkMap HumanReadablePart.fromList HumanReadablePart.toList

instance Arbitrary Word5 where
  arbitrary = arbitraryBoundedEnum
  shrink = shrinkBoundedEnum

instance Arbitrary a => Arbitrary (List.NonEmpty a) where
  arbitrary = (:|) <$> arbitrary <*> arbitrary
  shrink (a :| as) = uncurry (:|) <$> shrink (a, as)

--------------------------------------------------------------------------------
-- Properties for DataChar
--------------------------------------------------------------------------------

prop_DataChar_fromWord5_toWord5 :: Word5 -> Property
prop_DataChar_fromWord5_toWord5 w =
  DataChar.toWord5 (DataChar.fromWord5 w) === w

prop_DataChar_toWord5_fromWord5 :: DataChar -> Property
prop_DataChar_toWord5_fromWord5 c =
  DataChar.fromWord5 (DataChar.toWord5 c) === c

prop_DataChar_toChar_fromCharMaybe :: DataChar -> Property
prop_DataChar_toChar_fromCharMaybe c =
  DataChar.fromCharMaybe (DataChar.toChar c) === Just c

--------------------------------------------------------------------------------
-- Properties for DataPart
--------------------------------------------------------------------------------

prop_DataPart_append_fromWordList :: [Word5] -> [Word5] -> Property
prop_DataPart_append_fromWordList ps qs =
  DataPart.fromWordList (ps <> qs)
    === (DataPart.fromWordList ps <> DataPart.fromWordList qs)

prop_DataPart_append_toWordList :: DataPart -> DataPart -> Property
prop_DataPart_append_toWordList p q =
  DataPart.toWordList (p <> q)
    === (DataPart.toWordList p <> DataPart.toWordList q)

prop_DataPart_toText_fromText :: DataPart -> Property
prop_DataPart_toText_fromText d =
  DataPart.fromText (DataPart.toText d) === Right d

prop_DataPart_toWordList_fromWordList :: DataPart -> Property
prop_DataPart_toWordList_fromWordList d =
  DataPart.fromWordList (DataPart.toWordList d) === d

prop_DataPart_fromWordList_toWordList :: [Word5] -> Property
prop_DataPart_fromWordList_toWordList ws =
  DataPart.toWordList (DataPart.fromWordList ws) === ws

--------------------------------------------------------------------------------
-- Properties for HumanReadableChar
--------------------------------------------------------------------------------

prop_HumanReadableChar_toChar_fromCharMaybe :: HumanReadableChar -> Property
prop_HumanReadableChar_toChar_fromCharMaybe c =
  HumanReadableChar.fromCharMaybe (HumanReadableChar.toChar c) === Just c

--------------------------------------------------------------------------------
-- Properties for HumanReadablePart
--------------------------------------------------------------------------------

prop_HumanReadablePart_append_fromList
  :: NonEmpty HumanReadableChar
  -> NonEmpty HumanReadableChar
  -> Property
prop_HumanReadablePart_append_fromList ps qs =
  HumanReadablePart.fromList (ps <> qs)
    === (HumanReadablePart.fromList ps <> HumanReadablePart.fromList qs)

prop_HumanReadablePart_append_toList
  :: HumanReadablePart
  -> HumanReadablePart
  -> Property
prop_HumanReadablePart_append_toList p q =
  HumanReadablePart.toList (p <> q)
    === (HumanReadablePart.toList p <> HumanReadablePart.toList q)

prop_HumanReadablePart_toText_fromText :: HumanReadablePart -> Property
prop_HumanReadablePart_toText_fromText d =
  HumanReadablePart.fromText (HumanReadablePart.toText d) === Right d

prop_HumanReadablePart_toList_fromList :: HumanReadablePart -> Property
prop_HumanReadablePart_toList_fromList d =
  HumanReadablePart.fromList (HumanReadablePart.toList d) === d

prop_HumanReadablePart_fromList_toList :: NonEmpty HumanReadableChar -> Property
prop_HumanReadablePart_fromList_toList ws =
  HumanReadablePart.toList (HumanReadablePart.fromList ws) === ws
