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
  | SuffixChar_a
  | SuffixChar_c
  | SuffixChar_d
  | SuffixChar_e
  | SuffixChar_f
  | SuffixChar_g
  | SuffixChar_h
  | SuffixChar_j
  | SuffixChar_k
  | SuffixChar_l
  | SuffixChar_m
  | SuffixChar_n
  | SuffixChar_p
  | SuffixChar_q
  | SuffixChar_r
  | SuffixChar_s
  | SuffixChar_t
  | SuffixChar_u
  | SuffixChar_v
  | SuffixChar_w
  | SuffixChar_x
  | SuffixChar_y
  | SuffixChar_z
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
  Word5_00000 -> SuffixChar_q
  Word5_00001 -> SuffixChar_p
  Word5_00010 -> SuffixChar_z
  Word5_00011 -> SuffixChar_r
  Word5_00100 -> SuffixChar_y
  Word5_00101 -> SuffixChar_9
  Word5_00110 -> SuffixChar_x
  Word5_00111 -> SuffixChar_8
  Word5_01000 -> SuffixChar_g
  Word5_01001 -> SuffixChar_f
  Word5_01010 -> SuffixChar_2
  Word5_01011 -> SuffixChar_t
  Word5_01100 -> SuffixChar_v
  Word5_01101 -> SuffixChar_d
  Word5_01110 -> SuffixChar_w
  Word5_01111 -> SuffixChar_0
  Word5_10000 -> SuffixChar_s
  Word5_10001 -> SuffixChar_3
  Word5_10010 -> SuffixChar_j
  Word5_10011 -> SuffixChar_n
  Word5_10100 -> SuffixChar_5
  Word5_10101 -> SuffixChar_4
  Word5_10110 -> SuffixChar_k
  Word5_10111 -> SuffixChar_h
  Word5_11000 -> SuffixChar_c
  Word5_11001 -> SuffixChar_e
  Word5_11010 -> SuffixChar_6
  Word5_11011 -> SuffixChar_m
  Word5_11100 -> SuffixChar_u
  Word5_11101 -> SuffixChar_a
  Word5_11110 -> SuffixChar_7
  Word5_11111 -> SuffixChar_l

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
  SuffixChar_a -> Word5_11101
  SuffixChar_c -> Word5_11000
  SuffixChar_d -> Word5_01101
  SuffixChar_e -> Word5_11001
  SuffixChar_f -> Word5_01001
  SuffixChar_g -> Word5_01000
  SuffixChar_h -> Word5_10111
  SuffixChar_j -> Word5_10010
  SuffixChar_k -> Word5_10110
  SuffixChar_l -> Word5_11111
  SuffixChar_m -> Word5_11011
  SuffixChar_n -> Word5_10011
  SuffixChar_p -> Word5_00001
  SuffixChar_q -> Word5_00000
  SuffixChar_r -> Word5_00011
  SuffixChar_s -> Word5_10000
  SuffixChar_t -> Word5_01011
  SuffixChar_u -> Word5_11100
  SuffixChar_v -> Word5_01100
  SuffixChar_w -> Word5_01110
  SuffixChar_x -> Word5_00110
  SuffixChar_y -> Word5_00100
  SuffixChar_z -> Word5_00010

type family KnownValidChar (c :: Char) :: Constraint where
  KnownValidChar c =
    ( KnownChar c
    , Assert (Not (FromCharMaybe c == Nothing)) (TypeError CharError)
    )

type CharError =
  TypeError.Text
    "A data character must one of [023456789acdefghjklmnpqrstuvwxyz]."

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
  FromCharMaybe 'a' = Just SuffixChar_a
  FromCharMaybe 'c' = Just SuffixChar_c
  FromCharMaybe 'd' = Just SuffixChar_d
  FromCharMaybe 'e' = Just SuffixChar_e
  FromCharMaybe 'f' = Just SuffixChar_f
  FromCharMaybe 'g' = Just SuffixChar_g
  FromCharMaybe 'h' = Just SuffixChar_h
  FromCharMaybe 'j' = Just SuffixChar_j
  FromCharMaybe 'k' = Just SuffixChar_k
  FromCharMaybe 'l' = Just SuffixChar_l
  FromCharMaybe 'm' = Just SuffixChar_m
  FromCharMaybe 'n' = Just SuffixChar_n
  FromCharMaybe 'p' = Just SuffixChar_p
  FromCharMaybe 'q' = Just SuffixChar_q
  FromCharMaybe 'r' = Just SuffixChar_r
  FromCharMaybe 's' = Just SuffixChar_s
  FromCharMaybe 't' = Just SuffixChar_t
  FromCharMaybe 'u' = Just SuffixChar_u
  FromCharMaybe 'v' = Just SuffixChar_v
  FromCharMaybe 'w' = Just SuffixChar_w
  FromCharMaybe 'x' = Just SuffixChar_x
  FromCharMaybe 'y' = Just SuffixChar_y
  FromCharMaybe 'z' = Just SuffixChar_z
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
  'a' -> Just SuffixChar_a
  'c' -> Just SuffixChar_c
  'd' -> Just SuffixChar_d
  'e' -> Just SuffixChar_e
  'f' -> Just SuffixChar_f
  'g' -> Just SuffixChar_g
  'h' -> Just SuffixChar_h
  'j' -> Just SuffixChar_j
  'k' -> Just SuffixChar_k
  'l' -> Just SuffixChar_l
  'm' -> Just SuffixChar_m
  'n' -> Just SuffixChar_n
  'p' -> Just SuffixChar_p
  'q' -> Just SuffixChar_q
  'r' -> Just SuffixChar_r
  's' -> Just SuffixChar_s
  't' -> Just SuffixChar_t
  'u' -> Just SuffixChar_u
  'v' -> Just SuffixChar_v
  'w' -> Just SuffixChar_w
  'x' -> Just SuffixChar_x
  'y' -> Just SuffixChar_y
  'z' -> Just SuffixChar_z
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
  SuffixChar_a -> 'a'
  SuffixChar_c -> 'c'
  SuffixChar_d -> 'd'
  SuffixChar_e -> 'e'
  SuffixChar_f -> 'f'
  SuffixChar_g -> 'g'
  SuffixChar_h -> 'h'
  SuffixChar_j -> 'j'
  SuffixChar_k -> 'k'
  SuffixChar_l -> 'l'
  SuffixChar_m -> 'm'
  SuffixChar_n -> 'n'
  SuffixChar_p -> 'p'
  SuffixChar_q -> 'q'
  SuffixChar_r -> 'r'
  SuffixChar_s -> 's'
  SuffixChar_t -> 't'
  SuffixChar_u -> 'u'
  SuffixChar_v -> 'v'
  SuffixChar_w -> 'w'
  SuffixChar_x -> 'x'
  SuffixChar_y -> 'y'
  SuffixChar_z -> 'z'
