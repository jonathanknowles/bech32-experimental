{-# OPTIONS_GHC -Wno-orphans #-}

module Codec.Bech32.PrefixSpec (spec) where

import Codec.Bech32.Prefix (Prefix)
import Codec.Bech32.Prefix qualified as Prefix
import Codec.Bech32.Prefix.Char (PrefixChar)
import Data.List.NonEmpty (NonEmpty)
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
  , semigroupLaws
  , showLaws
  , showReadLaws
  )

spec :: Spec
spec = do
  testLaws @Prefix
    [ eqLaws
    , ordLaws
    , semigroupLaws
    , showLaws
    , showReadLaws
    ]
  prop "prop_append_fromList"
    \\\ prop_append_fromList
  prop "prop_append_toList"
    \\\ prop_append_toList
  prop "prop_toText_fromText"
    \\\ prop_toText_fromText
  prop "prop_fromList_toList"
    \\\ prop_fromList_toList
  prop "prop_toList_fromList"
    \\\ prop_toList_fromList

(\\\) :: (a -> b) -> a -> b
(\\\) = ($)

--------------------------------------------------------------------------------
-- Arbitrary instances
--------------------------------------------------------------------------------

instance Arbitrary PrefixChar where
  arbitrary = arbitraryBoundedEnum
  shrink = shrinkBoundedEnum

instance Arbitrary Prefix where
  arbitrary = Prefix.fromList <$> arbitrary
  shrink = shrinkMap Prefix.fromList Prefix.toList

--------------------------------------------------------------------------------
-- Properties
--------------------------------------------------------------------------------

prop_append_fromList
  :: NonEmpty PrefixChar
  -> NonEmpty PrefixChar
  -> Property
prop_append_fromList ps qs =
  Prefix.fromList (ps <> qs)
    === (Prefix.fromList ps <> Prefix.fromList qs)

prop_append_toList
  :: Prefix
  -> Prefix
  -> Property
prop_append_toList p q =
  Prefix.toList (p <> q)
    === (Prefix.toList p <> Prefix.toList q)

prop_toText_fromText :: Prefix -> Property
prop_toText_fromText d =
  Prefix.fromText (Prefix.toText d) === Right d

prop_toList_fromList :: Prefix -> Property
prop_toList_fromList d =
  Prefix.fromList (Prefix.toList d) === d

prop_fromList_toList :: NonEmpty PrefixChar -> Property
prop_fromList_toList ws =
  Prefix.toList (Prefix.fromList ws) === ws
