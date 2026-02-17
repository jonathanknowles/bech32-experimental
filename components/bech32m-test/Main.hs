{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Main (main) where

import Data.Bech32.DataChar (DataChar)
import Data.Bech32.HumanReadableChar (HumanReadableChar)
import Data.Bech32.HumanReadablePart (HumanReadablePart)
import Data.Bech32.HumanReadablePart qualified as HumanReadablePart
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as List (NonEmpty)
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
  , ordLaws
  , semigroupLaws
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
      , ordLaws
      , showLaws
      , showReadLaws
      ]
    testLawsMany @HumanReadableChar
      [ boundedEnumLaws
      , eqLaws
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

instance Arbitrary DataChar where
  arbitrary = arbitraryBoundedEnum
  shrink = shrinkBoundedEnum

instance Arbitrary HumanReadableChar where
  arbitrary = arbitraryBoundedEnum
  shrink = shrinkBoundedEnum

instance Arbitrary HumanReadablePart where
  arbitrary = HumanReadablePart.fromList <$> arbitrary
  shrink = shrinkMap HumanReadablePart.fromList HumanReadablePart.toList

instance Arbitrary a => Arbitrary (List.NonEmpty a) where
  arbitrary = (:|) <$> arbitrary <*> arbitrary
  shrink (a :| as) = uncurry (:|) <$> shrink (a, as)
