{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module Data.Word3
  ( Word3 (..)
  , fromIntegral
  , toWord8
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

data Word3
  = Word3_000
  | Word3_001
  | Word3_010
  | Word3_011
  | Word3_100
  | Word3_101
  | Word3_110
  | Word3_111
  deriving stock (Bounded, Enum, Eq, Generic, Ix, Ord, Read, Show)
  deriving anyclass Finitary

-- | Arithmetic modulo 8.
instance Num Word3 where
  a + b = fromIntegral (fromEnum a + fromEnum b)
  a * b = fromIntegral (fromEnum a * fromEnum b)
  a - b = fromIntegral (fromEnum a - fromEnum b)
  abs a = a
  fromInteger = fromIntegral
  signum 0 = 0
  signum _ = 1

instance Bits Word3 where
  (.&.) a b = fromWord8 (toWord8 a .&. toWord8 b)
  (.|.) a b = fromWord8 (toWord8 a .|. toWord8 b)
  xor a b = fromWord8 (toWord8 a `xor` toWord8 b)
  complement = fromWord8 . complement . toWord8
  popCount = popCount . toWord8
  bitSizeMaybe _ = Just 3
  isSigned _ = False
  bitSize _ = 3
  rotate a i =
    fromWord8 ((x `shiftL` r) .|. (x `shiftR` (3 - r)))
    where
      r = i `mod` 3
      x = toWord8 a
  shift a i
    | i >= 0 =
        fromWord8 (toWord8 a `shiftL` i)
    | otherwise =
        fromWord8 (toWord8 a `shiftR` negate i)
  bit i
    | 0 <= i && i < 3 =
        fromWord8 (bit i)
    | otherwise =
        zeroBits
  testBit a i
    | 0 <= i && i < 3 =
        testBit (toWord8 a) i
    | otherwise =
        False

instance FiniteBits Word3 where
  finiteBitSize _ = 3

fromWord8 :: Word8 -> Word3
fromWord8 = fromIntegral

toWord8 :: Word3 -> Word8
toWord8 = toEnum . fromEnum

-- | Creates a 'Word3' from an integral number (modulo 8).
fromIntegral :: Integral i => i -> Word3
fromIntegral i = toEnum (Prelude.fromIntegral (i `mod` 8))
