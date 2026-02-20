{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

{- HLINT ignore "Use camelCase" -}

module Codec.Bech32.DataChar
  ( DataChar
  , fromChar
  , fromCharMaybe
  , fromWord5
  , toChar
  , toWord5
  , ValidChar
  )
where

import Data.Finitary (Finitary)
import Data.Ix (Ix)
import Data.Kind (Constraint)
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy (Proxy))
import Data.Type.Bool (Not)
import Data.Type.Equality (type (==))
import Data.Word5 (Word5 (..))
import GHC.Generics (Generic)
import GHC.TypeError (Assert, TypeError)
import GHC.TypeError qualified as TypeError
import GHC.TypeLits (KnownChar, charVal)
import Text.Read (Lexeme (Ident, Punc), lexP, parens, prec, readPrec)

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
  deriving stock (Bounded, Enum, Eq, Generic, Ix, Ord)
  deriving anyclass Finitary

instance Read DataChar where
  readPrec = parens $ prec 10 $ do
    Ident "fromChar" <- lexP
    Punc "@" <- lexP
    unsafeFromChar <$> readPrec

instance Show DataChar where
  showsPrec d hrc =
    showParen (d > 10) $
      showString "fromChar @" . shows (toChar hrc)

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

type family KnownValidChar (c :: Char) :: Constraint where
  KnownValidChar c =
    ( KnownChar c
    , Assert (Not (FromCharMaybe c == Nothing)) (TypeError CharError)
    )

type CharError =
  TypeError.Text
    "A data character must one of [023456789ACDEFGHJKLMNPQRSTUVWXYZ]."

type family ValidChar (c :: Char) :: Bool where
  ValidChar c = Not (FromCharMaybe c == Nothing)

type family FromCharMaybe (c :: Char) :: Maybe DataChar where
  FromCharMaybe '0' = Just DataChar_0
  FromCharMaybe '2' = Just DataChar_2
  FromCharMaybe '3' = Just DataChar_3
  FromCharMaybe '4' = Just DataChar_4
  FromCharMaybe '5' = Just DataChar_5
  FromCharMaybe '6' = Just DataChar_6
  FromCharMaybe '7' = Just DataChar_7
  FromCharMaybe '8' = Just DataChar_8
  FromCharMaybe '9' = Just DataChar_9
  FromCharMaybe 'A' = Just DataChar_A
  FromCharMaybe 'C' = Just DataChar_C
  FromCharMaybe 'D' = Just DataChar_D
  FromCharMaybe 'E' = Just DataChar_E
  FromCharMaybe 'F' = Just DataChar_F
  FromCharMaybe 'G' = Just DataChar_G
  FromCharMaybe 'H' = Just DataChar_H
  FromCharMaybe 'J' = Just DataChar_J
  FromCharMaybe 'K' = Just DataChar_K
  FromCharMaybe 'L' = Just DataChar_L
  FromCharMaybe 'M' = Just DataChar_M
  FromCharMaybe 'N' = Just DataChar_N
  FromCharMaybe 'P' = Just DataChar_P
  FromCharMaybe 'Q' = Just DataChar_Q
  FromCharMaybe 'R' = Just DataChar_R
  FromCharMaybe 'S' = Just DataChar_S
  FromCharMaybe 'T' = Just DataChar_T
  FromCharMaybe 'U' = Just DataChar_U
  FromCharMaybe 'V' = Just DataChar_V
  FromCharMaybe 'W' = Just DataChar_W
  FromCharMaybe 'X' = Just DataChar_X
  FromCharMaybe 'Y' = Just DataChar_Y
  FromCharMaybe 'Z' = Just DataChar_Z
  FromCharMaybe _ = Nothing

fromChar :: forall c. KnownValidChar c => DataChar
fromChar =
  fromMaybe unexpectedOutOfRange $ fromCharMaybe $ charVal $ Proxy @c
  where
    unexpectedOutOfRange = error "DataChar.fromChar"

fromCharMaybe :: Char -> Maybe DataChar
fromCharMaybe = \case
  '0' -> Just DataChar_0
  '2' -> Just DataChar_2
  '3' -> Just DataChar_3
  '4' -> Just DataChar_4
  '5' -> Just DataChar_5
  '6' -> Just DataChar_6
  '7' -> Just DataChar_7
  '8' -> Just DataChar_8
  '9' -> Just DataChar_9
  'A' -> Just DataChar_A
  'C' -> Just DataChar_C
  'D' -> Just DataChar_D
  'E' -> Just DataChar_E
  'F' -> Just DataChar_F
  'G' -> Just DataChar_G
  'H' -> Just DataChar_H
  'J' -> Just DataChar_J
  'K' -> Just DataChar_K
  'L' -> Just DataChar_L
  'M' -> Just DataChar_M
  'N' -> Just DataChar_N
  'P' -> Just DataChar_P
  'Q' -> Just DataChar_Q
  'R' -> Just DataChar_R
  'S' -> Just DataChar_S
  'T' -> Just DataChar_T
  'U' -> Just DataChar_U
  'V' -> Just DataChar_V
  'W' -> Just DataChar_W
  'X' -> Just DataChar_X
  'Y' -> Just DataChar_Y
  'Z' -> Just DataChar_Z
  _ -> Nothing

unsafeFromChar :: Char -> DataChar
unsafeFromChar = fromMaybe onFailure . fromCharMaybe
  where
    onFailure = error "unsafeFromChar"

toChar :: DataChar -> Char
toChar = \case
  DataChar_0 -> '0'
  DataChar_2 -> '2'
  DataChar_3 -> '3'
  DataChar_4 -> '4'
  DataChar_5 -> '5'
  DataChar_6 -> '6'
  DataChar_7 -> '7'
  DataChar_8 -> '8'
  DataChar_9 -> '9'
  DataChar_A -> 'A'
  DataChar_C -> 'C'
  DataChar_D -> 'D'
  DataChar_E -> 'E'
  DataChar_F -> 'F'
  DataChar_G -> 'G'
  DataChar_H -> 'H'
  DataChar_J -> 'J'
  DataChar_K -> 'K'
  DataChar_L -> 'L'
  DataChar_M -> 'M'
  DataChar_N -> 'N'
  DataChar_P -> 'P'
  DataChar_Q -> 'Q'
  DataChar_R -> 'R'
  DataChar_S -> 'S'
  DataChar_T -> 'T'
  DataChar_U -> 'U'
  DataChar_V -> 'V'
  DataChar_W -> 'W'
  DataChar_X -> 'X'
  DataChar_Y -> 'Y'
  DataChar_Z -> 'Z'
