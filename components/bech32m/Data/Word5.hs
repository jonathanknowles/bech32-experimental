{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module Data.Word5
  ( Word5 (..)
  , fromIntegral
  )
where

import Data.Bits
  ( Bits
      ( bit
      , bitSize
      , bitSizeMaybe
      , complement
      , isSigned
      , popCount
      , rotate
      , shift
      , shiftL
      , shiftR
      , testBit
      , xor
      , zeroBits
      , (.&.)
      , (.|.)
      )
  , FiniteBits (finiteBitSize)
  )
import Data.Finitary (Finitary)
import Data.Ix (Ix)
import Data.Word (Word8)
import GHC.Generics (Generic)
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
  deriving stock (Bounded, Enum, Eq, Generic, Ix, Ord, Read, Show)
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

instance Bits Word5 where
  (.&.) a b = fromWord8 (toWord8 a .&. toWord8 b)
  (.|.) a b = fromWord8 (toWord8 a .|. toWord8 b)
  xor a b = fromWord8 (toWord8 a `xor` toWord8 b)
  complement = fromWord8 . complement . toWord8
  popCount = popCount . toWord8
  bitSizeMaybe _ = Just 5
  isSigned _ = False
  bitSize _ = 5
  rotate a i =
    fromWord8 ((x `shiftL` r) .|. (x `shiftR` (5 - r)))
    where
      r = i `mod` 5
      x = toWord8 a
  shift a i
    | i >= 0 =
        fromWord8 (toWord8 a `shiftL` i)
    | otherwise =
        fromWord8 (toWord8 a `shiftR` negate i)
  bit i
    | 0 <= i && i < 5 =
        fromWord8 (bit i)
    | otherwise =
        zeroBits
  testBit a i
    | 0 <= i && i < 5 =
        testBit (toWord8 a) i
    | otherwise =
        False

instance FiniteBits Word5 where
  finiteBitSize _ = 5

fromWord8 :: Word8 -> Word5
fromWord8 = fromIntegral

toWord8 :: Word5 -> Word8
toWord8 = toEnum . fromEnum

-- | Creates a 'Word5' from an integral number (modulo 32).
fromIntegral :: Integral i => i -> Word5
fromIntegral i = toEnum (Prelude.fromIntegral (i `mod` 32))
