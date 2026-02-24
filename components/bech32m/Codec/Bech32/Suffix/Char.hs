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

module Codec.Bech32.Suffix.Char
  ( SuffixChar
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

data SuffixChar
  = SuffixChar_0
  | SuffixChar_2
  | SuffixChar_3
  | SuffixChar_4
  | SuffixChar_5
  | SuffixChar_6
  | SuffixChar_7
  | SuffixChar_8
  | SuffixChar_9
  | SuffixChar_A
  | SuffixChar_C
  | SuffixChar_D
  | SuffixChar_E
  | SuffixChar_F
  | SuffixChar_G
  | SuffixChar_H
  | SuffixChar_J
  | SuffixChar_K
  | SuffixChar_L
  | SuffixChar_M
  | SuffixChar_N
  | SuffixChar_P
  | SuffixChar_Q
  | SuffixChar_R
  | SuffixChar_S
  | SuffixChar_T
  | SuffixChar_U
  | SuffixChar_V
  | SuffixChar_W
  | SuffixChar_X
  | SuffixChar_Y
  | SuffixChar_Z
  deriving stock (Bounded, Enum, Eq, Generic, Ix, Ord)
  deriving anyclass Finitary

instance Read SuffixChar where
  readPrec = parens $ prec 10 $ do
    Ident "fromChar" <- lexP
    Punc "@" <- lexP
    unsafeFromChar <$> readPrec

instance Show SuffixChar where
  showsPrec d hrc =
    showParen (d > 10) $
      showString "fromChar @" . shows (toChar hrc)

fromWord5 :: Word5 -> SuffixChar
fromWord5 = \case
  Word5_00000 -> SuffixChar_Q
  Word5_00001 -> SuffixChar_P
  Word5_00010 -> SuffixChar_Z
  Word5_00011 -> SuffixChar_R
  Word5_00100 -> SuffixChar_Y
  Word5_00101 -> SuffixChar_9
  Word5_00110 -> SuffixChar_X
  Word5_00111 -> SuffixChar_8
  Word5_01000 -> SuffixChar_G
  Word5_01001 -> SuffixChar_F
  Word5_01010 -> SuffixChar_2
  Word5_01011 -> SuffixChar_T
  Word5_01100 -> SuffixChar_V
  Word5_01101 -> SuffixChar_D
  Word5_01110 -> SuffixChar_W
  Word5_01111 -> SuffixChar_0
  Word5_10000 -> SuffixChar_S
  Word5_10001 -> SuffixChar_3
  Word5_10010 -> SuffixChar_J
  Word5_10011 -> SuffixChar_N
  Word5_10100 -> SuffixChar_5
  Word5_10101 -> SuffixChar_4
  Word5_10110 -> SuffixChar_K
  Word5_10111 -> SuffixChar_H
  Word5_11000 -> SuffixChar_C
  Word5_11001 -> SuffixChar_E
  Word5_11010 -> SuffixChar_6
  Word5_11011 -> SuffixChar_M
  Word5_11100 -> SuffixChar_U
  Word5_11101 -> SuffixChar_A
  Word5_11110 -> SuffixChar_7
  Word5_11111 -> SuffixChar_L

toWord5 :: SuffixChar -> Word5
toWord5 = \case
  SuffixChar_0 -> Word5_01111
  SuffixChar_2 -> Word5_01010
  SuffixChar_3 -> Word5_10001
  SuffixChar_4 -> Word5_10101
  SuffixChar_5 -> Word5_10100
  SuffixChar_6 -> Word5_11010
  SuffixChar_7 -> Word5_11110
  SuffixChar_8 -> Word5_00111
  SuffixChar_9 -> Word5_00101
  SuffixChar_A -> Word5_11101
  SuffixChar_C -> Word5_11000
  SuffixChar_D -> Word5_01101
  SuffixChar_E -> Word5_11001
  SuffixChar_F -> Word5_01001
  SuffixChar_G -> Word5_01000
  SuffixChar_H -> Word5_10111
  SuffixChar_J -> Word5_10010
  SuffixChar_K -> Word5_10110
  SuffixChar_L -> Word5_11111
  SuffixChar_M -> Word5_11011
  SuffixChar_N -> Word5_10011
  SuffixChar_P -> Word5_00001
  SuffixChar_Q -> Word5_00000
  SuffixChar_R -> Word5_00011
  SuffixChar_S -> Word5_10000
  SuffixChar_T -> Word5_01011
  SuffixChar_U -> Word5_11100
  SuffixChar_V -> Word5_01100
  SuffixChar_W -> Word5_01110
  SuffixChar_X -> Word5_00110
  SuffixChar_Y -> Word5_00100
  SuffixChar_Z -> Word5_00010

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

type family FromCharMaybe (c :: Char) :: Maybe SuffixChar where
  FromCharMaybe '0' = Just SuffixChar_0
  FromCharMaybe '2' = Just SuffixChar_2
  FromCharMaybe '3' = Just SuffixChar_3
  FromCharMaybe '4' = Just SuffixChar_4
  FromCharMaybe '5' = Just SuffixChar_5
  FromCharMaybe '6' = Just SuffixChar_6
  FromCharMaybe '7' = Just SuffixChar_7
  FromCharMaybe '8' = Just SuffixChar_8
  FromCharMaybe '9' = Just SuffixChar_9
  FromCharMaybe 'A' = Just SuffixChar_A
  FromCharMaybe 'C' = Just SuffixChar_C
  FromCharMaybe 'D' = Just SuffixChar_D
  FromCharMaybe 'E' = Just SuffixChar_E
  FromCharMaybe 'F' = Just SuffixChar_F
  FromCharMaybe 'G' = Just SuffixChar_G
  FromCharMaybe 'H' = Just SuffixChar_H
  FromCharMaybe 'J' = Just SuffixChar_J
  FromCharMaybe 'K' = Just SuffixChar_K
  FromCharMaybe 'L' = Just SuffixChar_L
  FromCharMaybe 'M' = Just SuffixChar_M
  FromCharMaybe 'N' = Just SuffixChar_N
  FromCharMaybe 'P' = Just SuffixChar_P
  FromCharMaybe 'Q' = Just SuffixChar_Q
  FromCharMaybe 'R' = Just SuffixChar_R
  FromCharMaybe 'S' = Just SuffixChar_S
  FromCharMaybe 'T' = Just SuffixChar_T
  FromCharMaybe 'U' = Just SuffixChar_U
  FromCharMaybe 'V' = Just SuffixChar_V
  FromCharMaybe 'W' = Just SuffixChar_W
  FromCharMaybe 'X' = Just SuffixChar_X
  FromCharMaybe 'Y' = Just SuffixChar_Y
  FromCharMaybe 'Z' = Just SuffixChar_Z
  FromCharMaybe _ = Nothing

fromChar :: forall c. KnownValidChar c => SuffixChar
fromChar =
  fromMaybe unexpectedOutOfRange $ fromCharMaybe $ charVal $ Proxy @c
  where
    unexpectedOutOfRange = error "SuffixChar.fromChar"

fromCharMaybe :: Char -> Maybe SuffixChar
fromCharMaybe = \case
  '0' -> Just SuffixChar_0
  '2' -> Just SuffixChar_2
  '3' -> Just SuffixChar_3
  '4' -> Just SuffixChar_4
  '5' -> Just SuffixChar_5
  '6' -> Just SuffixChar_6
  '7' -> Just SuffixChar_7
  '8' -> Just SuffixChar_8
  '9' -> Just SuffixChar_9
  'A' -> Just SuffixChar_A
  'C' -> Just SuffixChar_C
  'D' -> Just SuffixChar_D
  'E' -> Just SuffixChar_E
  'F' -> Just SuffixChar_F
  'G' -> Just SuffixChar_G
  'H' -> Just SuffixChar_H
  'J' -> Just SuffixChar_J
  'K' -> Just SuffixChar_K
  'L' -> Just SuffixChar_L
  'M' -> Just SuffixChar_M
  'N' -> Just SuffixChar_N
  'P' -> Just SuffixChar_P
  'Q' -> Just SuffixChar_Q
  'R' -> Just SuffixChar_R
  'S' -> Just SuffixChar_S
  'T' -> Just SuffixChar_T
  'U' -> Just SuffixChar_U
  'V' -> Just SuffixChar_V
  'W' -> Just SuffixChar_W
  'X' -> Just SuffixChar_X
  'Y' -> Just SuffixChar_Y
  'Z' -> Just SuffixChar_Z
  _ -> Nothing

unsafeFromChar :: Char -> SuffixChar
unsafeFromChar = fromMaybe onFailure . fromCharMaybe
  where
    onFailure = error "unsafeFromChar"

toChar :: SuffixChar -> Char
toChar = \case
  SuffixChar_0 -> '0'
  SuffixChar_2 -> '2'
  SuffixChar_3 -> '3'
  SuffixChar_4 -> '4'
  SuffixChar_5 -> '5'
  SuffixChar_6 -> '6'
  SuffixChar_7 -> '7'
  SuffixChar_8 -> '8'
  SuffixChar_9 -> '9'
  SuffixChar_A -> 'A'
  SuffixChar_C -> 'C'
  SuffixChar_D -> 'D'
  SuffixChar_E -> 'E'
  SuffixChar_F -> 'F'
  SuffixChar_G -> 'G'
  SuffixChar_H -> 'H'
  SuffixChar_J -> 'J'
  SuffixChar_K -> 'K'
  SuffixChar_L -> 'L'
  SuffixChar_M -> 'M'
  SuffixChar_N -> 'N'
  SuffixChar_P -> 'P'
  SuffixChar_Q -> 'Q'
  SuffixChar_R -> 'R'
  SuffixChar_S -> 'S'
  SuffixChar_T -> 'T'
  SuffixChar_U -> 'U'
  SuffixChar_V -> 'V'
  SuffixChar_W -> 'W'
  SuffixChar_X -> 'X'
  SuffixChar_Y -> 'Y'
  SuffixChar_Z -> 'Z'
