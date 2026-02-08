{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}

{- HLINT ignore "Use camelCase" -}

module Data.Bech32.DataChar
  ( fromChar
  , fromWord5
  , toChar
  , toWord5
  )
where

import Data.Finitary (Finitary)
import Data.Ix (Ix)
import Data.Word5 (Word5 (..))
import GHC.Generics (Generic)
import Text.Read (readMaybe)

data DataChar
  = DataChar_0
  | DataChar_2
  | DataChar_3
  | DataChar_4
  | DataChar_5
  | DataChar_6
  | DataChar_7
  | DataChar_8
  | DataChar_9
  | DataChar_A
  | DataChar_C
  | DataChar_D
  | DataChar_E
  | DataChar_F
  | DataChar_G
  | DataChar_H
  | DataChar_J
  | DataChar_K
  | DataChar_L
  | DataChar_M
  | DataChar_N
  | DataChar_P
  | DataChar_Q
  | DataChar_R
  | DataChar_S
  | DataChar_T
  | DataChar_U
  | DataChar_V
  | DataChar_W
  | DataChar_X
  | DataChar_Y
  | DataChar_Z
  deriving stock (Bounded, Enum, Eq, Generic, Ix, Ord, Read, Show)
  deriving anyclass Finitary

fromWord5 :: Word5 -> DataChar
fromWord5 = \case
  Word5_00 -> DataChar_Q
  Word5_01 -> DataChar_P
  Word5_02 -> DataChar_Z
  Word5_03 -> DataChar_R
  Word5_04 -> DataChar_Y
  Word5_05 -> DataChar_9
  Word5_06 -> DataChar_X
  Word5_07 -> DataChar_8
  Word5_08 -> DataChar_G
  Word5_09 -> DataChar_F
  Word5_10 -> DataChar_2
  Word5_11 -> DataChar_T
  Word5_12 -> DataChar_V
  Word5_13 -> DataChar_D
  Word5_14 -> DataChar_W
  Word5_15 -> DataChar_0
  Word5_16 -> DataChar_S
  Word5_17 -> DataChar_3
  Word5_18 -> DataChar_J
  Word5_19 -> DataChar_N
  Word5_20 -> DataChar_5
  Word5_21 -> DataChar_4
  Word5_22 -> DataChar_K
  Word5_23 -> DataChar_H
  Word5_24 -> DataChar_C
  Word5_25 -> DataChar_E
  Word5_26 -> DataChar_6
  Word5_27 -> DataChar_M
  Word5_28 -> DataChar_U
  Word5_29 -> DataChar_A
  Word5_30 -> DataChar_7
  Word5_31 -> DataChar_L

toWord5 :: DataChar -> Word5
toWord5 = \case
  DataChar_0 -> Word5_15
  DataChar_2 -> Word5_10
  DataChar_3 -> Word5_17
  DataChar_4 -> Word5_21
  DataChar_5 -> Word5_20
  DataChar_6 -> Word5_26
  DataChar_7 -> Word5_30
  DataChar_8 -> Word5_07
  DataChar_9 -> Word5_05
  DataChar_A -> Word5_29
  DataChar_C -> Word5_24
  DataChar_D -> Word5_13
  DataChar_E -> Word5_25
  DataChar_F -> Word5_09
  DataChar_G -> Word5_08
  DataChar_H -> Word5_23
  DataChar_J -> Word5_18
  DataChar_K -> Word5_22
  DataChar_L -> Word5_31
  DataChar_M -> Word5_27
  DataChar_N -> Word5_19
  DataChar_P -> Word5_01
  DataChar_Q -> Word5_00
  DataChar_R -> Word5_03
  DataChar_S -> Word5_16
  DataChar_T -> Word5_11
  DataChar_U -> Word5_28
  DataChar_V -> Word5_12
  DataChar_W -> Word5_14
  DataChar_X -> Word5_06
  DataChar_Y -> Word5_04
  DataChar_Z -> Word5_02

fromChar :: Char -> Maybe DataChar
fromChar c = readMaybe ("DataChar_" <> [c])

toChar :: DataChar -> Char
toChar c = last (show c)
