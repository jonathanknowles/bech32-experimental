{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module Data.Word2
  ( Word2 (..)
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

data Word2
  = Word2_00
  | Word2_01
  | Word2_10
  | Word2_11
  deriving stock (Bounded, Enum, Eq, Generic, Ix, Ord, Read, Show)
  deriving anyclass Finitary

-- | Arithmetic modulo 4.
instance Num Word2 where
  a + b = fromIntegral (fromEnum a + fromEnum b)
  a * b = fromIntegral (fromEnum a * fromEnum b)
  a - b = fromIntegral (fromEnum a - fromEnum b)
  abs a = a
  fromInteger = fromIntegral
  signum 0 = 0
  signum _ = 1

instance Bits Word2 where
  (.&.) a b = fromWord8 (toWord8 a .&. toWord8 b)
  (.|.) a b = fromWord8 (toWord8 a .|. toWord8 b)
  xor a b = fromWord8 (toWord8 a `xor` toWord8 b)
  complement = fromWord8 . complement . toWord8
  popCount = popCount . toWord8
  bitSizeMaybe _ = Just 2
  isSigned _ = False
  bitSize _ = 2
  rotate a i =
    fromWord8 ((x `shiftL` r) .|. (x `shiftR` (2 - r)))
    where
      r = i `mod` 2
      x = toWord8 a
  shift a i
    | i >= 0 =
        fromWord8 (toWord8 a `shiftL` i)
    | otherwise =
        fromWord8 (toWord8 a `shiftR` negate i)
  bit i
    | 0 <= i && i < 2 =
        fromWord8 (bit i)
    | otherwise =
        zeroBits
  testBit a i
    | 0 <= i && i < 2 =
        testBit (toWord8 a) i
    | otherwise =
        False

instance FiniteBits Word2 where
  finiteBitSize _ = 2

fromWord8 :: Word8 -> Word2
fromWord8 = fromIntegral

toWord8 :: Word2 -> Word8
toWord8 = toEnum . fromEnum

-- | Creates a 'Word2' from an integral number (modulo 4).
fromIntegral :: Integral i => i -> Word2
fromIntegral i = toEnum (Prelude.fromIntegral (i `mod` 4))
