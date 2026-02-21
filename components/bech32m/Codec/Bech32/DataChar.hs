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
  Word5_00000 -> DataChar_Q
  Word5_00001 -> DataChar_P
  Word5_00010 -> DataChar_Z
  Word5_00011 -> DataChar_R
  Word5_00100 -> DataChar_Y
  Word5_00101 -> DataChar_9
  Word5_00110 -> DataChar_X
  Word5_00111 -> DataChar_8
  Word5_01000 -> DataChar_G
  Word5_01001 -> DataChar_F
  Word5_01010 -> DataChar_2
  Word5_01011 -> DataChar_T
  Word5_01100 -> DataChar_V
  Word5_01101 -> DataChar_D
  Word5_01110 -> DataChar_W
  Word5_01111 -> DataChar_0
  Word5_10000 -> DataChar_S
  Word5_10001 -> DataChar_3
  Word5_10010 -> DataChar_J
  Word5_10011 -> DataChar_N
  Word5_10100 -> DataChar_5
  Word5_10101 -> DataChar_4
  Word5_10110 -> DataChar_K
  Word5_10111 -> DataChar_H
  Word5_11000 -> DataChar_C
  Word5_11001 -> DataChar_E
  Word5_11010 -> DataChar_6
  Word5_11011 -> DataChar_M
  Word5_11100 -> DataChar_U
  Word5_11101 -> DataChar_A
  Word5_11110 -> DataChar_7
  Word5_11111 -> DataChar_L

toWord5 :: DataChar -> Word5
toWord5 = \case
  DataChar_0 -> Word5_01111
  DataChar_2 -> Word5_01010
  DataChar_3 -> Word5_10001
  DataChar_4 -> Word5_10101
  DataChar_5 -> Word5_10100
  DataChar_6 -> Word5_11010
  DataChar_7 -> Word5_11110
  DataChar_8 -> Word5_00111
  DataChar_9 -> Word5_00101
  DataChar_A -> Word5_11101
  DataChar_C -> Word5_11000
  DataChar_D -> Word5_01101
  DataChar_E -> Word5_11001
  DataChar_F -> Word5_01001
  DataChar_G -> Word5_01000
  DataChar_H -> Word5_10111
  DataChar_J -> Word5_10010
  DataChar_K -> Word5_10110
  DataChar_L -> Word5_11111
  DataChar_M -> Word5_11011
  DataChar_N -> Word5_10011
  DataChar_P -> Word5_00001
  DataChar_Q -> Word5_00000
  DataChar_R -> Word5_00011
  DataChar_S -> Word5_10000
  DataChar_T -> Word5_01011
  DataChar_U -> Word5_11100
  DataChar_V -> Word5_01100
  DataChar_W -> Word5_01110
  DataChar_X -> Word5_00110
  DataChar_Y -> Word5_00100
  DataChar_Z -> Word5_00010

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
