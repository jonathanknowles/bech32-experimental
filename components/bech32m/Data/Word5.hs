{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module Data.Word5 where

import Data.Finitary (Finitary)
import Data.Ix (Ix)
import GHC.Generics (Generic)
import Text.Read (Read (readPrec))
import Prelude hiding (fromIntegral)
import Prelude qualified

data Word5
  = Word5_00000
  | Word5_00001
  | Word5_00010
  | Word5_00011
  | Word5_00100
  | Word5_00101
  | Word5_00110
  | Word5_00111
  | Word5_01000
  | Word5_01001
  | Word5_01010
  | Word5_01011
  | Word5_01100
  | Word5_01101
  | Word5_01110
  | Word5_01111
  | Word5_10000
  | Word5_10001
  | Word5_10010
  | Word5_10011
  | Word5_10100
  | Word5_10101
  | Word5_10110
  | Word5_10111
  | Word5_11000
  | Word5_11001
  | Word5_11010
  | Word5_11011
  | Word5_11100
  | Word5_11101
  | Word5_11110
  | Word5_11111
  deriving stock (Bounded, Enum, Eq, Generic, Ix, Ord)
  deriving anyclass Finitary

-- | Arithmetic modulo 32.
instance Num Word5 where
  a + b = fromIntegral (fromEnum a + fromEnum b)
  a * b = fromIntegral (fromEnum a * fromEnum b)
  a - b = fromIntegral (fromEnum a - fromEnum b)
  abs a = a
  fromInteger = fromIntegral
  signum 0 = 0
  signum _ = 1

instance Read Word5 where readPrec = fromInteger <$> readPrec

instance Show Word5 where show = show . fromEnum

-- | Creates a 'Word5' from an integral number (modulo 32).
fromIntegral :: Integral i => i -> Word5
fromIntegral i = toEnum (Prelude.fromIntegral (i `mod` 32))
