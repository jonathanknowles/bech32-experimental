{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Codec.Bech32.Prefix.CharSpec (spec) where

import Codec.Bech32.Prefix.Char (PrefixChar)
import Codec.Bech32.Prefix.Char qualified as PrefixChar
import Test.Hspec (Spec)
import Test.Hspec.QuickCheck (prop)
import Test.Hspec.QuickCheck.Classes (testLaws)
import Test.QuickCheck
  ( Arbitrary (arbitrary, shrink)
  , Property
  , arbitraryBoundedEnum
  , shrinkBoundedEnum
  , (===)
  )
import Test.QuickCheck.Arbitrary ()
import Test.QuickCheck.Classes
  ( boundedEnumLaws
  , eqLaws
  , ixLaws
  , ordLaws
  , showLaws
  , showReadLaws
  )

spec :: Spec
spec = do
  testLaws @PrefixChar
    [ boundedEnumLaws
    , eqLaws
    , ixLaws
    , ordLaws
    , showLaws
    , showReadLaws
    ]
  prop "prop_toChar_fromCharMaybe"
    \\\ prop_toChar_fromCharMaybe
  prop "prop_toOrdinal_fromOrdinalMaybe"
    \\\ prop_toOrdinal_fromOrdinalMaybe

(\\\) :: (a -> b) -> a -> b
(\\\) = ($)

--------------------------------------------------------------------------------
-- Arbitrary instances
--------------------------------------------------------------------------------

instance Arbitrary PrefixChar where
  arbitrary = arbitraryBoundedEnum
  shrink = shrinkBoundedEnum

--------------------------------------------------------------------------------
-- Properties
--------------------------------------------------------------------------------

prop_toChar_fromCharMaybe :: PrefixChar -> Property
prop_toChar_fromCharMaybe c =
  PrefixChar.fromCharMaybe (PrefixChar.toChar c) === Just c

prop_toOrdinal_fromOrdinalMaybe :: PrefixChar -> Property
prop_toOrdinal_fromOrdinalMaybe c =
  PrefixChar.fromOrdinalMaybe (PrefixChar.toOrdinal c) === Just c
