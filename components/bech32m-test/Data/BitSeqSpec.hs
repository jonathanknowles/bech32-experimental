{-# OPTIONS_GHC -Wno-orphans #-}

module Data.BitSeqSpec
  ( spec
  ) where

import Data.Bit (Bit)
import Data.BitSeq qualified as BitSeq
import Test.Hspec (Spec, describe, it)
import Test.QuickCheck
  ( Arbitrary (arbitrary, shrink)
  , Property
  , Testable (property)
  , arbitraryBoundedEnum
  , shrinkBoundedEnum
  , (===)
  )

spec :: Spec
spec = describe "Properties" $ do
  it "prop_fromList_toList" $
    property
      prop_fromList_toList

--------------------------------------------------------------------------------
-- Arbitrary instances
--------------------------------------------------------------------------------

instance Arbitrary Bit where
  arbitrary = arbitraryBoundedEnum
  shrink = shrinkBoundedEnum

--------------------------------------------------------------------------------
-- Properties
--------------------------------------------------------------------------------

prop_fromList_toList :: [Bit] -> Property
prop_fromList_toList bits =
  BitSeq.toList (BitSeq.fromList bits) === bits
