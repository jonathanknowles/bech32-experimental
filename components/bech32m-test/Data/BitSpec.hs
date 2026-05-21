{-# OPTIONS_GHC -Wno-orphans #-}

module Data.BitSpec (spec) where

import Data.Bit (Bit)
import Test.Hspec (Spec)
import Test.Hspec.QuickCheck.Classes (testLaws)
import Test.QuickCheck
  ( Arbitrary (arbitrary, shrink)
  , arbitraryBoundedEnum
  , shrinkBoundedEnum
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

--------------------------------------------------------------------------------
-- Arbitrary instances
--------------------------------------------------------------------------------

instance Arbitrary Bit where
  arbitrary = arbitraryBoundedEnum
  shrink = shrinkBoundedEnum
