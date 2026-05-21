{-# OPTIONS_GHC -Wno-orphans #-}

module Data.WordSpec (spec) where

import Data.Word2 (Word2)
import Data.Word3 (Word3)
import Data.Word5 (Word5)
import Test.Hspec (Spec)
import Test.Hspec.QuickCheck.Classes (testLaws)
import Test.QuickCheck
  ( Arbitrary (arbitrary, shrink)
  , arbitraryBoundedEnum
  , shrinkBoundedEnum
  )
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

instance Arbitrary Word2 where
  arbitrary = arbitraryBoundedEnum
  shrink = shrinkBoundedEnum

instance Arbitrary Word3 where
  arbitrary = arbitraryBoundedEnum
  shrink = shrinkBoundedEnum

instance Arbitrary Word5 where
  arbitrary = arbitraryBoundedEnum
  shrink = shrinkBoundedEnum
