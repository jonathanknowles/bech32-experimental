{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Codec.Bech32.HumanReadableChar
  ( HumanReadableChar
  , fromChar
  , fromCharMaybe
  , toChar
  , ValidChar
  )
where

import Data.Finitary (Finitary)
import Data.Ix (Ix)
import Data.Kind (Constraint)
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy (Proxy))
import Data.Type.Bool (Not, type (&&))
import Data.Type.Equality (type (==))
import GHC.Generics (Generic)
import GHC.TypeError (Assert, TypeError)
import GHC.TypeError qualified as TypeError
import GHC.TypeLits (CmpChar, KnownChar, charVal)
import Text.Read (Lexeme (Ident, Punc), Read (readPrec), lexP, parens, prec)
import Prelude

data HumanReadableChar
  = HumanReadableChar_033
  | HumanReadableChar_034
  | HumanReadableChar_035
  | HumanReadableChar_036
  | HumanReadableChar_037
  | HumanReadableChar_038
  | HumanReadableChar_039
  | HumanReadableChar_040
  | HumanReadableChar_041
  | HumanReadableChar_042
  | HumanReadableChar_043
  | HumanReadableChar_044
  | HumanReadableChar_045
  | HumanReadableChar_046
  | HumanReadableChar_047
  | HumanReadableChar_048
  | HumanReadableChar_049
  | HumanReadableChar_050
  | HumanReadableChar_051
  | HumanReadableChar_052
  | HumanReadableChar_053
  | HumanReadableChar_054
  | HumanReadableChar_055
  | HumanReadableChar_056
  | HumanReadableChar_057
  | HumanReadableChar_058
  | HumanReadableChar_059
  | HumanReadableChar_060
  | HumanReadableChar_061
  | HumanReadableChar_062
  | HumanReadableChar_063
  | HumanReadableChar_064
  | HumanReadableChar_065
  | HumanReadableChar_066
  | HumanReadableChar_067
  | HumanReadableChar_068
  | HumanReadableChar_069
  | HumanReadableChar_070
  | HumanReadableChar_071
  | HumanReadableChar_072
  | HumanReadableChar_073
  | HumanReadableChar_074
  | HumanReadableChar_075
  | HumanReadableChar_076
  | HumanReadableChar_077
  | HumanReadableChar_078
  | HumanReadableChar_079
  | HumanReadableChar_080
  | HumanReadableChar_081
  | HumanReadableChar_082
  | HumanReadableChar_083
  | HumanReadableChar_084
  | HumanReadableChar_085
  | HumanReadableChar_086
  | HumanReadableChar_087
  | HumanReadableChar_088
  | HumanReadableChar_089
  | HumanReadableChar_090
  | HumanReadableChar_091
  | HumanReadableChar_092
  | HumanReadableChar_093
  | HumanReadableChar_094
  | HumanReadableChar_095
  | HumanReadableChar_096
  | HumanReadableChar_097
  | HumanReadableChar_098
  | HumanReadableChar_099
  | HumanReadableChar_100
  | HumanReadableChar_101
  | HumanReadableChar_102
  | HumanReadableChar_103
  | HumanReadableChar_104
  | HumanReadableChar_105
  | HumanReadableChar_106
  | HumanReadableChar_107
  | HumanReadableChar_108
  | HumanReadableChar_109
  | HumanReadableChar_110
  | HumanReadableChar_111
  | HumanReadableChar_112
  | HumanReadableChar_113
  | HumanReadableChar_114
  | HumanReadableChar_115
  | HumanReadableChar_116
  | HumanReadableChar_117
  | HumanReadableChar_118
  | HumanReadableChar_119
  | HumanReadableChar_120
  | HumanReadableChar_121
  | HumanReadableChar_122
  | HumanReadableChar_123
  | HumanReadableChar_124
  | HumanReadableChar_125
  | HumanReadableChar_126
  deriving stock (Bounded, Enum, Eq, Generic, Ix, Ord)
  deriving anyclass Finitary

instance Read HumanReadableChar where
  readPrec = parens $ prec 10 $ do
    Ident "fromChar" <- lexP
    Punc "@" <- lexP
    unsafeFromChar <$> readPrec

instance Show HumanReadableChar where
  showsPrec d hrc =
    showParen (d > 10) $
      showString "fromChar @" . shows (toChar hrc)

type family KnownValidChar (c :: Char) :: Constraint where
  KnownValidChar c =
    ( KnownChar c
    , Assert (ValidChar c) (TypeError CharError)
    )

type family ValidChar (c :: Char) :: Bool where
  ValidChar c =
    (&&)
      (Not (CmpChar c '!' == 'LT))
      (Not (CmpChar c '~' == 'GT))

type CharError =
  TypeError.Text
    "A HumanReadableChar must be a character in the range ['!' .. '~']."

fromChar :: forall c. KnownValidChar c => HumanReadableChar
fromChar =
  fromMaybe unexpectedOutOfRange $ fromCharMaybe $ charVal $ Proxy @c
  where
    unexpectedOutOfRange = error "HumanReadableChar.fromChar"

fromCharMaybe :: Char -> Maybe HumanReadableChar
fromCharMaybe = \case
  '!' -> Just HumanReadableChar_033
  '"' -> Just HumanReadableChar_034
  '#' -> Just HumanReadableChar_035
  '$' -> Just HumanReadableChar_036
  '%' -> Just HumanReadableChar_037
  '&' -> Just HumanReadableChar_038
  '\'' -> Just HumanReadableChar_039
  '(' -> Just HumanReadableChar_040
  ')' -> Just HumanReadableChar_041
  '*' -> Just HumanReadableChar_042
  '+' -> Just HumanReadableChar_043
  ',' -> Just HumanReadableChar_044
  '-' -> Just HumanReadableChar_045
  '.' -> Just HumanReadableChar_046
  '/' -> Just HumanReadableChar_047
  '0' -> Just HumanReadableChar_048
  '1' -> Just HumanReadableChar_049
  '2' -> Just HumanReadableChar_050
  '3' -> Just HumanReadableChar_051
  '4' -> Just HumanReadableChar_052
  '5' -> Just HumanReadableChar_053
  '6' -> Just HumanReadableChar_054
  '7' -> Just HumanReadableChar_055
  '8' -> Just HumanReadableChar_056
  '9' -> Just HumanReadableChar_057
  ':' -> Just HumanReadableChar_058
  ';' -> Just HumanReadableChar_059
  '<' -> Just HumanReadableChar_060
  '=' -> Just HumanReadableChar_061
  '>' -> Just HumanReadableChar_062
  '?' -> Just HumanReadableChar_063
  '@' -> Just HumanReadableChar_064
  'A' -> Just HumanReadableChar_065
  'B' -> Just HumanReadableChar_066
  'C' -> Just HumanReadableChar_067
  'D' -> Just HumanReadableChar_068
  'E' -> Just HumanReadableChar_069
  'F' -> Just HumanReadableChar_070
  'G' -> Just HumanReadableChar_071
  'H' -> Just HumanReadableChar_072
  'I' -> Just HumanReadableChar_073
  'J' -> Just HumanReadableChar_074
  'K' -> Just HumanReadableChar_075
  'L' -> Just HumanReadableChar_076
  'M' -> Just HumanReadableChar_077
  'N' -> Just HumanReadableChar_078
  'O' -> Just HumanReadableChar_079
  'P' -> Just HumanReadableChar_080
  'Q' -> Just HumanReadableChar_081
  'R' -> Just HumanReadableChar_082
  'S' -> Just HumanReadableChar_083
  'T' -> Just HumanReadableChar_084
  'U' -> Just HumanReadableChar_085
  'V' -> Just HumanReadableChar_086
  'W' -> Just HumanReadableChar_087
  'X' -> Just HumanReadableChar_088
  'Y' -> Just HumanReadableChar_089
  'Z' -> Just HumanReadableChar_090
  '[' -> Just HumanReadableChar_091
  '\\' -> Just HumanReadableChar_092
  ']' -> Just HumanReadableChar_093
  '^' -> Just HumanReadableChar_094
  '_' -> Just HumanReadableChar_095
  '`' -> Just HumanReadableChar_096
  'a' -> Just HumanReadableChar_097
  'b' -> Just HumanReadableChar_098
  'c' -> Just HumanReadableChar_099
  'd' -> Just HumanReadableChar_100
  'e' -> Just HumanReadableChar_101
  'f' -> Just HumanReadableChar_102
  'g' -> Just HumanReadableChar_103
  'h' -> Just HumanReadableChar_104
  'i' -> Just HumanReadableChar_105
  'j' -> Just HumanReadableChar_106
  'k' -> Just HumanReadableChar_107
  'l' -> Just HumanReadableChar_108
  'm' -> Just HumanReadableChar_109
  'n' -> Just HumanReadableChar_110
  'o' -> Just HumanReadableChar_111
  'p' -> Just HumanReadableChar_112
  'q' -> Just HumanReadableChar_113
  'r' -> Just HumanReadableChar_114
  's' -> Just HumanReadableChar_115
  't' -> Just HumanReadableChar_116
  'u' -> Just HumanReadableChar_117
  'v' -> Just HumanReadableChar_118
  'w' -> Just HumanReadableChar_119
  'x' -> Just HumanReadableChar_120
  'y' -> Just HumanReadableChar_121
  'z' -> Just HumanReadableChar_122
  '{' -> Just HumanReadableChar_123
  '|' -> Just HumanReadableChar_124
  '}' -> Just HumanReadableChar_125
  '~' -> Just HumanReadableChar_126
  _ -> Nothing

unsafeFromChar :: Char -> HumanReadableChar
unsafeFromChar = fromMaybe onFailure . fromCharMaybe
  where
    onFailure = error "unsafeFromChar"

toChar :: HumanReadableChar -> Char
toChar = \case
  HumanReadableChar_033 -> '!'
  HumanReadableChar_034 -> '"'
  HumanReadableChar_035 -> '#'
  HumanReadableChar_036 -> '$'
  HumanReadableChar_037 -> '%'
  HumanReadableChar_038 -> '&'
  HumanReadableChar_039 -> '\''
  HumanReadableChar_040 -> '('
  HumanReadableChar_041 -> ')'
  HumanReadableChar_042 -> '*'
  HumanReadableChar_043 -> '+'
  HumanReadableChar_044 -> ','
  HumanReadableChar_045 -> '-'
  HumanReadableChar_046 -> '.'
  HumanReadableChar_047 -> '/'
  HumanReadableChar_048 -> '0'
  HumanReadableChar_049 -> '1'
  HumanReadableChar_050 -> '2'
  HumanReadableChar_051 -> '3'
  HumanReadableChar_052 -> '4'
  HumanReadableChar_053 -> '5'
  HumanReadableChar_054 -> '6'
  HumanReadableChar_055 -> '7'
  HumanReadableChar_056 -> '8'
  HumanReadableChar_057 -> '9'
  HumanReadableChar_058 -> ':'
  HumanReadableChar_059 -> ';'
  HumanReadableChar_060 -> '<'
  HumanReadableChar_061 -> '='
  HumanReadableChar_062 -> '>'
  HumanReadableChar_063 -> '?'
  HumanReadableChar_064 -> '@'
  HumanReadableChar_065 -> 'A'
  HumanReadableChar_066 -> 'B'
  HumanReadableChar_067 -> 'C'
  HumanReadableChar_068 -> 'D'
  HumanReadableChar_069 -> 'E'
  HumanReadableChar_070 -> 'F'
  HumanReadableChar_071 -> 'G'
  HumanReadableChar_072 -> 'H'
  HumanReadableChar_073 -> 'I'
  HumanReadableChar_074 -> 'J'
  HumanReadableChar_075 -> 'K'
  HumanReadableChar_076 -> 'L'
  HumanReadableChar_077 -> 'M'
  HumanReadableChar_078 -> 'N'
  HumanReadableChar_079 -> 'O'
  HumanReadableChar_080 -> 'P'
  HumanReadableChar_081 -> 'Q'
  HumanReadableChar_082 -> 'R'
  HumanReadableChar_083 -> 'S'
  HumanReadableChar_084 -> 'T'
  HumanReadableChar_085 -> 'U'
  HumanReadableChar_086 -> 'V'
  HumanReadableChar_087 -> 'W'
  HumanReadableChar_088 -> 'X'
  HumanReadableChar_089 -> 'Y'
  HumanReadableChar_090 -> 'Z'
  HumanReadableChar_091 -> '['
  HumanReadableChar_092 -> '\\'
  HumanReadableChar_093 -> ']'
  HumanReadableChar_094 -> '^'
  HumanReadableChar_095 -> '_'
  HumanReadableChar_096 -> '`'
  HumanReadableChar_097 -> 'a'
  HumanReadableChar_098 -> 'b'
  HumanReadableChar_099 -> 'c'
  HumanReadableChar_100 -> 'd'
  HumanReadableChar_101 -> 'e'
  HumanReadableChar_102 -> 'f'
  HumanReadableChar_103 -> 'g'
  HumanReadableChar_104 -> 'h'
  HumanReadableChar_105 -> 'i'
  HumanReadableChar_106 -> 'j'
  HumanReadableChar_107 -> 'k'
  HumanReadableChar_108 -> 'l'
  HumanReadableChar_109 -> 'm'
  HumanReadableChar_110 -> 'n'
  HumanReadableChar_111 -> 'o'
  HumanReadableChar_112 -> 'p'
  HumanReadableChar_113 -> 'q'
  HumanReadableChar_114 -> 'r'
  HumanReadableChar_115 -> 's'
  HumanReadableChar_116 -> 't'
  HumanReadableChar_117 -> 'u'
  HumanReadableChar_118 -> 'v'
  HumanReadableChar_119 -> 'w'
  HumanReadableChar_120 -> 'x'
  HumanReadableChar_121 -> 'y'
  HumanReadableChar_122 -> 'z'
  HumanReadableChar_123 -> '{'
  HumanReadableChar_124 -> '|'
  HumanReadableChar_125 -> '}'
  HumanReadableChar_126 -> '~'
