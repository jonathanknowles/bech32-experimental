{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module OtherSpec (spec) where

import Codec.Bech32.Prefix (Prefix)
import Codec.Bech32.Prefix qualified as Prefix
import Codec.Bech32.Prefix.Char (PrefixChar)
import Codec.Bech32.Suffix.Char (SuffixChar)
import Codec.Bech32.Suffix.Payload (Payload)
import Codec.Bech32.Suffix.Payload qualified as Payload
import Data.Bit (Bit)
import Data.Word2 (Word2)
import Data.Word3 (Word3)
import Data.Word5 (Word5)
import Test.Hspec (Spec, describe)
import Test.Hspec.QuickCheck.Classes (testLaws)
import Test.QuickCheck
  ( Arbitrary (arbitrary, shrink)
  , arbitraryBoundedEnum
  , shrinkBoundedEnum
  , shrinkMap
  )
import Test.QuickCheck.Arbitrary ()
import Test.QuickCheck.Classes
  ( bitsLaws
  , boundedEnumLaws
  , eqLaws
  , ixLaws
  , numLaws
  , ordLaws
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
