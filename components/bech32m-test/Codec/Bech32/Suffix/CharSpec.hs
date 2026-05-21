{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Codec.Bech32.Suffix.CharSpec (spec) where

import Codec.Bech32.Suffix.Char (SuffixChar)
import Codec.Bech32.Suffix.Char qualified as SuffixChar
import Data.Word5 (Word5)
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
  testLaws @SuffixChar
    [ boundedEnumLaws
    , eqLaws
    , ixLaws
    , ordLaws
    , showLaws
    , showReadLaws
    ]
  prop "prop_fromWord5_toWord5"
    \\\ prop_fromWord5_toWord5
  prop "prop_toChar_fromCharMaybe"
    \\\ prop_toChar_fromCharMaybe
  prop "prop_toWord5_fromWord5"
    \\\ prop_toWord5_fromWord5

(\\\) :: (a -> b) -> a -> b
(\\\) = ($)

--------------------------------------------------------------------------------
-- Arbitrary instances
--------------------------------------------------------------------------------

instance Arbitrary SuffixChar where
  arbitrary = arbitraryBoundedEnum
  shrink = shrinkBoundedEnum

instance Arbitrary Word5 where
  arbitrary = arbitraryBoundedEnum
  shrink = shrinkBoundedEnum

--------------------------------------------------------------------------------
-- Properties
--------------------------------------------------------------------------------

prop_fromWord5_toWord5 :: Word5 -> Property
prop_fromWord5_toWord5 w =
  SuffixChar.toWord5 (SuffixChar.fromWord5 w) === w

prop_toChar_fromCharMaybe :: SuffixChar -> Property
prop_toChar_fromCharMaybe c =
  SuffixChar.fromCharMaybe (SuffixChar.toChar c) === Just c

prop_toWord5_fromWord5 :: SuffixChar -> Property
prop_toWord5_fromWord5 c =
  SuffixChar.fromWord5 (SuffixChar.toWord5 c) === c
