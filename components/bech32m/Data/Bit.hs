{-# LANGUAGE DeriveGeneric #-}

module Data.Bit
  ( Bit (..)
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
      , testBit
      , xor
      , zeroBits
      , (.&.)
      , (.|.)
      )
  , FiniteBits (finiteBitSize)
  )
import Data.Ix (Ix)
import GHC.Generics (Generic)

data Bit = B0 | B1
  deriving (Bounded, Enum, Eq, Generic, Ix, Ord, Read, Show)

instance Num Bit where
  abs = id
  negate = id
  signum = id

  fromInteger n = if even n then B0 else B1

  B0 + B0 = B0
  B0 + B1 = B1
  B1 + B0 = B1
  B1 + B1 = B0

  B0 * B0 = B0
  B0 * B1 = B0
  B1 * B0 = B0
  B1 * B1 = B1

instance Bits Bit where
  zeroBits = B0

  B0 .&. _ = B0
  B1 .&. b = b

  B1 .|. _ = B1
  B0 .|. b = b

  xor B0 b = b
  xor B1 b = complement b

  complement B0 = B1
  complement B1 = B0

  shift b 0 = b
  shift _ _ = B0

  rotate b _ = b

  bitSize _ = 1
  bitSizeMaybe _ = Just 1

  isSigned _ = False

  testBit B1 0 = True
  testBit _ _ = False

  bit 0 = B1
  bit _ = B0

  popCount B0 = 0
  popCount B1 = 1

instance FiniteBits Bit where
  finiteBitSize _ = 1
