{-# OPTIONS_GHC -Wno-orphans #-}

module Codec.Bech32.SuffixSpec (spec) where

import Codec.Bech32.Suffix (Suffix (..))
import Codec.Bech32.Suffix qualified as Suffix
import Codec.Bech32.Suffix.Checksum (Checksum (..))
import Codec.Bech32.Suffix.Payload (Payload)
import Codec.Bech32.Suffix.Payload qualified as Payload
import Data.Word5 (Word5)
import Test.Hspec (Spec)
import Test.Hspec.QuickCheck (prop)
import Test.Hspec.QuickCheck.Classes (testLaws)
import Test.QuickCheck
  ( Arbitrary (arbitrary, shrink)
  , Property
  , arbitraryBoundedEnum
  , shrinkBoundedEnum
  , shrinkMap
  , (===)
  )
import Test.QuickCheck.Arbitrary ()
import Test.QuickCheck.Classes
  ( eqLaws
  , ordLaws
  , showLaws
  , showReadLaws
  )

spec :: Spec
spec = do
  testLaws @Suffix
    [ eqLaws
    , ordLaws
    , showLaws
    , showReadLaws
    ]
  prop "prop_toText_fromText"
    \\\ prop_toText_fromText

(\\\) :: (a -> b) -> a -> b
(\\\) = ($)

--------------------------------------------------------------------------------
-- Arbitrary instances
--------------------------------------------------------------------------------

instance Arbitrary Checksum where
  arbitrary = tupleToChecksum <$> arbitrary
  shrink = shrinkMap tupleToChecksum checksumToTuple

checksumToTuple :: Checksum -> (Word5, Word5, Word5, Word5, Word5, Word5)
checksumToTuple Checksum {c0, c1, c2, c3, c4, c5} = (c0, c1, c2, c3, c4, c5)

tupleToChecksum :: (Word5, Word5, Word5, Word5, Word5, Word5) -> Checksum
tupleToChecksum (c0, c1, c2, c3, c4, c5) = Checksum {c0, c1, c2, c3, c4, c5}

instance Arbitrary Payload where
  arbitrary = Payload.fromWord5List <$> arbitrary
  shrink = shrinkMap Payload.fromWord5List Payload.toWord5List

instance Arbitrary Word5 where
  arbitrary = arbitraryBoundedEnum
  shrink = shrinkBoundedEnum

instance Arbitrary Suffix where
  arbitrary = tupleToSuffix <$> arbitrary
  shrink = shrinkMap tupleToSuffix suffixToTuple

suffixToTuple :: Suffix -> (Payload, Checksum)
suffixToTuple Suffix {payload, checksum} = (payload, checksum)

tupleToSuffix :: (Payload, Checksum) -> Suffix
tupleToSuffix (payload, checksum) = Suffix {payload, checksum}

--------------------------------------------------------------------------------
-- Properties
--------------------------------------------------------------------------------

prop_toText_fromText :: Suffix -> Property
prop_toText_fromText d =
  Suffix.fromText (Suffix.toText d) === Right d
