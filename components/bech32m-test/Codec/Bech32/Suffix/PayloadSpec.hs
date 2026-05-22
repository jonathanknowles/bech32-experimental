{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeAbstractions #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Codec.Bech32.Suffix.PayloadSpec
  ( spec
  ) where

import Codec.Bech32.Suffix.Payload
  ( PaddingError (PaddingNonZero, PaddingTooLong)
  , Payload
  )
import Codec.Bech32.Suffix.Payload qualified as Payload
import Data.Bits (FiniteBits (countTrailingZeros))
import Data.Maybe (listToMaybe)
import Data.Word (Word8)
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
import Test.QuickCheck.Classes
  ( eqLaws
  , monoidLaws
  , ordLaws
  , semigroupLaws
  , semigroupMonoidLaws
  , showLaws
  , showReadLaws
  )
import Prelude hiding (words)

spec :: Spec
spec = do
  testLaws @Payload
    [ eqLaws
    , ordLaws
    , showLaws
    , showReadLaws
    , semigroupLaws
    , monoidLaws
    , semigroupMonoidLaws
    ]

  prop "prop_fromWord5List_append"
    \\\ prop_fromWord5List_append
  prop "prop_fromWord5List_toWord5List"
    \\\ prop_fromWord5List_toWord5List
  prop "prop_fromWord8List_toWord8List"
    \\\ prop_fromWord8List_toWord8List
  prop "prop_toText_fromText"
    \\\ prop_toText_fromText
  prop "prop_toWord5List_append"
    \\\ prop_toWord5List_append
  prop "prop_toWord5List_fromWord5List"
    \\\ prop_toWord5List_fromWord5List
  prop "prop_toWord8List_fromWord8List"
    \\\ prop_toWord8List_fromWord8List

(\\\) :: (a -> b) -> a -> b
(\\\) = ($)

--------------------------------------------------------------------------------
-- Arbitrary instances
--------------------------------------------------------------------------------

instance Arbitrary Payload where
  arbitrary = Payload.fromWord5List <$> arbitrary
  shrink = shrinkMap Payload.fromWord5List Payload.toWord5List

instance Arbitrary Word5 where
  arbitrary = arbitraryBoundedEnum
  shrink = shrinkBoundedEnum

--------------------------------------------------------------------------------
-- Properties
--------------------------------------------------------------------------------

prop_fromWord5List_append :: [Word5] -> [Word5] -> Property
prop_fromWord5List_append ps qs =
  Payload.fromWord5List ps <> Payload.fromWord5List qs
    === Payload.fromWord5List (ps <> qs)

prop_fromWord5List_toWord5List :: [Word5] -> Property
prop_fromWord5List_toWord5List ws =
  Payload.toWord5List (Payload.fromWord5List ws) === ws

prop_fromWord8List_toWord8List :: [Word8] -> Property
prop_fromWord8List_toWord8List ws =
  Payload.toWord8List (Payload.fromWord8List ws) === Right ws

prop_toText_fromText :: Payload -> Property
prop_toText_fromText p =
  Payload.fromText (Payload.toText p) === Right p

prop_toWord5List_append :: Payload -> Payload -> Property
prop_toWord5List_append p q =
  Payload.toWord5List p <> Payload.toWord5List q
    === Payload.toWord5List (p <> q)

prop_toWord5List_fromWord5List :: Payload -> Property
prop_toWord5List_fromWord5List p =
  Payload.fromWord5List (Payload.toWord5List p) === p

prop_toWord8List_fromWord8List :: Payload -> Property
prop_toWord8List_fromWord8List p
  | paddingLength > 4 =
      Payload.toWord8List p === Left PaddingTooLong
  | Just lastWord <- maybeLastWord
  , countTrailingZeros lastWord < paddingLength =
      Payload.toWord8List p === Left PaddingNonZero
  | otherwise =
      fmap Payload.fromWord8List (Payload.toWord8List p) === Right p
  where
    maybeLastWord = listToMaybe $ reverse $ Payload.toWord5List p
    paddingLength = (Payload.length p * 5) `mod` 8

-- Padding is too long if (Payload.length p `mod` 8) `elem` [1, 3, 6]
--
-- Word8 count | Word5 count | Padding bits
--
--  0             0            0
--  1             2            2
--  2             4            4
--  3             5            1
--  4             7            3
--  5             8            0
--  6            10            2
--  7            12            4
--  8            13            1
--  9            15            3
-- 10            16            0
