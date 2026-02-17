{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Main (main) where

import Data.Bech32.DataChar (DataChar)
import Data.Bech32.DataPart (DataPart)
import Data.Bech32.DataPart qualified as DataPart
import Data.Bech32.HumanReadableChar (HumanReadableChar)
import Data.Bech32.HumanReadablePart (HumanReadablePart)
import Data.Bech32.HumanReadablePart qualified as HumanReadablePart
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as List (NonEmpty)
import Data.Word5 (Word5)
import Test.Hspec (describe, hspec)
import Test.QuickCheck
  ( Arbitrary (arbitrary, shrink)
  , arbitraryBoundedEnum
  , shrinkBoundedEnum
  , shrinkMap
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
  , showReadLaws
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
      [ boundedEnumLaws
      , eqLaws
      , ixLaws
      , numLaws
      , ordLaws
      , showLaws
      , showReadLaws
      ]

instance Arbitrary DataChar where
  arbitrary = arbitraryBoundedEnum
  shrink = shrinkBoundedEnum

instance Arbitrary DataPart where
  arbitrary = DataPart.fromList <$> arbitrary
  shrink = shrinkMap DataPart.fromList DataPart.toList

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
