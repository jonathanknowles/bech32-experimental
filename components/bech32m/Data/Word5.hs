{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module Data.Word5 where

import Data.Finitary (Finitary)
import Data.Ix (Ix)
import GHC.Generics (Generic)
import Text.Read (Read (readPrec))
import Prelude hiding (fromIntegral)
import qualified Prelude

data Word5
  = Word5_00
  | Word5_01
  | Word5_02
  | Word5_03
  | Word5_04
  | Word5_05
  | Word5_06
  | Word5_07
  | Word5_08
  | Word5_09
  | Word5_10
  | Word5_11
  | Word5_12
  | Word5_13
  | Word5_14
  | Word5_15
  | Word5_16
  | Word5_17
  | Word5_18
  | Word5_19
  | Word5_20
  | Word5_21
  | Word5_22
  | Word5_23
  | Word5_24
  | Word5_25
  | Word5_26
  | Word5_27
  | Word5_28
  | Word5_29
  | Word5_30
  | Word5_31
  deriving stock (Bounded, Enum, Eq, Generic, Ix, Ord)
  deriving anyclass Finitary

-- | Arithmetic modulo 32.
instance Num Word5 where
  a + b = fromIntegral (fromEnum a + fromEnum b)
  a * b = fromIntegral (fromEnum a * fromEnum b)
  a - b = fromIntegral (fromEnum a - fromEnum b)
  abs a = a
  fromInteger = fromIntegral
  signum Word5_00 = Word5_00
  signum __ = Word5_01

instance Read Word5 where readPrec = fromInteger <$> readPrec

instance Show Word5 where show = show . fromEnum

-- | Creates a 'Word5' from an integral number (modulo 32).
fromIntegral :: Integral i => i -> Word5
fromIntegral i = toEnum (Prelude.fromIntegral (i `mod` 32))
