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

module Codec.Bech32.Prefix.Char
  ( PrefixChar
  , fromChar
  , fromCharMaybe
  , toChar
  , toOrdinal
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
import Data.Word (Word8)
import GHC.Generics (Generic)
import GHC.TypeError (Assert, TypeError)
import GHC.TypeError qualified as TypeError
import GHC.TypeLits (CmpChar, KnownChar, charVal)
import Text.Read (Lexeme (Ident, Punc), Read (readPrec), lexP, parens, prec)
import Prelude

data PrefixChar
  = PrefixChar_033
  | PrefixChar_034
  | PrefixChar_035
  | PrefixChar_036
  | PrefixChar_037
  | PrefixChar_038
  | PrefixChar_039
  | PrefixChar_040
  | PrefixChar_041
  | PrefixChar_042
  | PrefixChar_043
  | PrefixChar_044
  | PrefixChar_045
  | PrefixChar_046
  | PrefixChar_047
  | PrefixChar_048
  | PrefixChar_049
  | PrefixChar_050
  | PrefixChar_051
  | PrefixChar_052
  | PrefixChar_053
  | PrefixChar_054
  | PrefixChar_055
  | PrefixChar_056
  | PrefixChar_057
  | PrefixChar_058
  | PrefixChar_059
  | PrefixChar_060
  | PrefixChar_061
  | PrefixChar_062
  | PrefixChar_063
  | PrefixChar_064
  | PrefixChar_065
  | PrefixChar_066
  | PrefixChar_067
  | PrefixChar_068
  | PrefixChar_069
  | PrefixChar_070
  | PrefixChar_071
  | PrefixChar_072
  | PrefixChar_073
  | PrefixChar_074
  | PrefixChar_075
  | PrefixChar_076
  | PrefixChar_077
  | PrefixChar_078
  | PrefixChar_079
  | PrefixChar_080
  | PrefixChar_081
  | PrefixChar_082
  | PrefixChar_083
  | PrefixChar_084
  | PrefixChar_085
  | PrefixChar_086
  | PrefixChar_087
  | PrefixChar_088
  | PrefixChar_089
  | PrefixChar_090
  | PrefixChar_091
  | PrefixChar_092
  | PrefixChar_093
  | PrefixChar_094
  | PrefixChar_095
  | PrefixChar_096
  | PrefixChar_097
  | PrefixChar_098
  | PrefixChar_099
  | PrefixChar_100
  | PrefixChar_101
  | PrefixChar_102
  | PrefixChar_103
  | PrefixChar_104
  | PrefixChar_105
  | PrefixChar_106
  | PrefixChar_107
  | PrefixChar_108
  | PrefixChar_109
  | PrefixChar_110
  | PrefixChar_111
  | PrefixChar_112
  | PrefixChar_113
  | PrefixChar_114
  | PrefixChar_115
  | PrefixChar_116
  | PrefixChar_117
  | PrefixChar_118
  | PrefixChar_119
  | PrefixChar_120
  | PrefixChar_121
  | PrefixChar_122
  | PrefixChar_123
  | PrefixChar_124
  | PrefixChar_125
  | PrefixChar_126
  deriving stock (Bounded, Enum, Eq, Generic, Ix, Ord)
  deriving anyclass Finitary

instance Read PrefixChar where
  readPrec = parens $ prec 10 $ do
    Ident "fromChar" <- lexP
    Punc "@" <- lexP
    unsafeFromChar <$> readPrec

instance Show PrefixChar where
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
    "A PrefixChar must be a character in the range ['!' .. '~']."

fromChar :: forall c. KnownValidChar c => PrefixChar
fromChar =
  fromMaybe unexpectedOutOfRange $ fromCharMaybe $ charVal $ Proxy @c
  where
    unexpectedOutOfRange = error "PrefixChar.fromChar"

fromCharMaybe :: Char -> Maybe PrefixChar
fromCharMaybe = \case
  '!' -> Just PrefixChar_033
  '"' -> Just PrefixChar_034
  '#' -> Just PrefixChar_035
  '$' -> Just PrefixChar_036
  '%' -> Just PrefixChar_037
  '&' -> Just PrefixChar_038
  '\'' -> Just PrefixChar_039
  '(' -> Just PrefixChar_040
  ')' -> Just PrefixChar_041
  '*' -> Just PrefixChar_042
  '+' -> Just PrefixChar_043
  ',' -> Just PrefixChar_044
  '-' -> Just PrefixChar_045
  '.' -> Just PrefixChar_046
  '/' -> Just PrefixChar_047
  '0' -> Just PrefixChar_048
  '1' -> Just PrefixChar_049
  '2' -> Just PrefixChar_050
  '3' -> Just PrefixChar_051
  '4' -> Just PrefixChar_052
  '5' -> Just PrefixChar_053
  '6' -> Just PrefixChar_054
  '7' -> Just PrefixChar_055
  '8' -> Just PrefixChar_056
  '9' -> Just PrefixChar_057
  ':' -> Just PrefixChar_058
  ';' -> Just PrefixChar_059
  '<' -> Just PrefixChar_060
  '=' -> Just PrefixChar_061
  '>' -> Just PrefixChar_062
  '?' -> Just PrefixChar_063
  '@' -> Just PrefixChar_064
  'A' -> Just PrefixChar_065
  'B' -> Just PrefixChar_066
  'C' -> Just PrefixChar_067
  'D' -> Just PrefixChar_068
  'E' -> Just PrefixChar_069
  'F' -> Just PrefixChar_070
  'G' -> Just PrefixChar_071
  'H' -> Just PrefixChar_072
  'I' -> Just PrefixChar_073
  'J' -> Just PrefixChar_074
  'K' -> Just PrefixChar_075
  'L' -> Just PrefixChar_076
  'M' -> Just PrefixChar_077
  'N' -> Just PrefixChar_078
  'O' -> Just PrefixChar_079
  'P' -> Just PrefixChar_080
  'Q' -> Just PrefixChar_081
  'R' -> Just PrefixChar_082
  'S' -> Just PrefixChar_083
  'T' -> Just PrefixChar_084
  'U' -> Just PrefixChar_085
  'V' -> Just PrefixChar_086
  'W' -> Just PrefixChar_087
  'X' -> Just PrefixChar_088
  'Y' -> Just PrefixChar_089
  'Z' -> Just PrefixChar_090
  '[' -> Just PrefixChar_091
  '\\' -> Just PrefixChar_092
  ']' -> Just PrefixChar_093
  '^' -> Just PrefixChar_094
  '_' -> Just PrefixChar_095
  '`' -> Just PrefixChar_096
  'a' -> Just PrefixChar_097
  'b' -> Just PrefixChar_098
  'c' -> Just PrefixChar_099
  'd' -> Just PrefixChar_100
  'e' -> Just PrefixChar_101
  'f' -> Just PrefixChar_102
  'g' -> Just PrefixChar_103
  'h' -> Just PrefixChar_104
  'i' -> Just PrefixChar_105
  'j' -> Just PrefixChar_106
  'k' -> Just PrefixChar_107
  'l' -> Just PrefixChar_108
  'm' -> Just PrefixChar_109
  'n' -> Just PrefixChar_110
  'o' -> Just PrefixChar_111
  'p' -> Just PrefixChar_112
  'q' -> Just PrefixChar_113
  'r' -> Just PrefixChar_114
  's' -> Just PrefixChar_115
  't' -> Just PrefixChar_116
  'u' -> Just PrefixChar_117
  'v' -> Just PrefixChar_118
  'w' -> Just PrefixChar_119
  'x' -> Just PrefixChar_120
  'y' -> Just PrefixChar_121
  'z' -> Just PrefixChar_122
  '{' -> Just PrefixChar_123
  '|' -> Just PrefixChar_124
  '}' -> Just PrefixChar_125
  '~' -> Just PrefixChar_126
  _ -> Nothing

unsafeFromChar :: Char -> PrefixChar
unsafeFromChar = fromMaybe onFailure . fromCharMaybe
  where
    onFailure = error "unsafeFromChar"

toOrdinal :: PrefixChar -> Word8
toOrdinal = \case
  PrefixChar_033 -> 033
  PrefixChar_034 -> 034
  PrefixChar_035 -> 035
  PrefixChar_036 -> 036
  PrefixChar_037 -> 037
  PrefixChar_038 -> 038
  PrefixChar_039 -> 039
  PrefixChar_040 -> 040
  PrefixChar_041 -> 041
  PrefixChar_042 -> 042
  PrefixChar_043 -> 043
  PrefixChar_044 -> 044
  PrefixChar_045 -> 045
  PrefixChar_046 -> 046
  PrefixChar_047 -> 047
  PrefixChar_048 -> 048
  PrefixChar_049 -> 049
  PrefixChar_050 -> 050
  PrefixChar_051 -> 051
  PrefixChar_052 -> 052
  PrefixChar_053 -> 053
  PrefixChar_054 -> 054
  PrefixChar_055 -> 055
  PrefixChar_056 -> 056
  PrefixChar_057 -> 057
  PrefixChar_058 -> 058
  PrefixChar_059 -> 059
  PrefixChar_060 -> 060
  PrefixChar_061 -> 061
  PrefixChar_062 -> 062
  PrefixChar_063 -> 063
  PrefixChar_064 -> 064
  PrefixChar_065 -> 065
  PrefixChar_066 -> 066
  PrefixChar_067 -> 067
  PrefixChar_068 -> 068
  PrefixChar_069 -> 069
  PrefixChar_070 -> 070
  PrefixChar_071 -> 071
  PrefixChar_072 -> 072
  PrefixChar_073 -> 073
  PrefixChar_074 -> 074
  PrefixChar_075 -> 075
  PrefixChar_076 -> 076
  PrefixChar_077 -> 077
  PrefixChar_078 -> 078
  PrefixChar_079 -> 079
  PrefixChar_080 -> 080
  PrefixChar_081 -> 081
  PrefixChar_082 -> 082
  PrefixChar_083 -> 083
  PrefixChar_084 -> 084
  PrefixChar_085 -> 085
  PrefixChar_086 -> 086
  PrefixChar_087 -> 087
  PrefixChar_088 -> 088
  PrefixChar_089 -> 089
  PrefixChar_090 -> 090
  PrefixChar_091 -> 091
  PrefixChar_092 -> 092
  PrefixChar_093 -> 093
  PrefixChar_094 -> 094
  PrefixChar_095 -> 095
  PrefixChar_096 -> 096
  PrefixChar_097 -> 097
  PrefixChar_098 -> 098
  PrefixChar_099 -> 099
  PrefixChar_100 -> 100
  PrefixChar_101 -> 101
  PrefixChar_102 -> 102
  PrefixChar_103 -> 103
  PrefixChar_104 -> 104
  PrefixChar_105 -> 105
  PrefixChar_106 -> 106
  PrefixChar_107 -> 107
  PrefixChar_108 -> 108
  PrefixChar_109 -> 109
  PrefixChar_110 -> 110
  PrefixChar_111 -> 111
  PrefixChar_112 -> 112
  PrefixChar_113 -> 113
  PrefixChar_114 -> 114
  PrefixChar_115 -> 115
  PrefixChar_116 -> 116
  PrefixChar_117 -> 117
  PrefixChar_118 -> 118
  PrefixChar_119 -> 119
  PrefixChar_120 -> 120
  PrefixChar_121 -> 121
  PrefixChar_122 -> 122
  PrefixChar_123 -> 123
  PrefixChar_124 -> 124
  PrefixChar_125 -> 125
  PrefixChar_126 -> 126

toChar :: PrefixChar -> Char
toChar = \case
  PrefixChar_033 -> '!'
  PrefixChar_034 -> '"'
  PrefixChar_035 -> '#'
  PrefixChar_036 -> '$'
  PrefixChar_037 -> '%'
  PrefixChar_038 -> '&'
  PrefixChar_039 -> '\''
  PrefixChar_040 -> '('
  PrefixChar_041 -> ')'
  PrefixChar_042 -> '*'
  PrefixChar_043 -> '+'
  PrefixChar_044 -> ','
  PrefixChar_045 -> '-'
  PrefixChar_046 -> '.'
  PrefixChar_047 -> '/'
  PrefixChar_048 -> '0'
  PrefixChar_049 -> '1'
  PrefixChar_050 -> '2'
  PrefixChar_051 -> '3'
  PrefixChar_052 -> '4'
  PrefixChar_053 -> '5'
  PrefixChar_054 -> '6'
  PrefixChar_055 -> '7'
  PrefixChar_056 -> '8'
  PrefixChar_057 -> '9'
  PrefixChar_058 -> ':'
  PrefixChar_059 -> ';'
  PrefixChar_060 -> '<'
  PrefixChar_061 -> '='
  PrefixChar_062 -> '>'
  PrefixChar_063 -> '?'
  PrefixChar_064 -> '@'
  PrefixChar_065 -> 'A'
  PrefixChar_066 -> 'B'
  PrefixChar_067 -> 'C'
  PrefixChar_068 -> 'D'
  PrefixChar_069 -> 'E'
  PrefixChar_070 -> 'F'
  PrefixChar_071 -> 'G'
  PrefixChar_072 -> 'H'
  PrefixChar_073 -> 'I'
  PrefixChar_074 -> 'J'
  PrefixChar_075 -> 'K'
  PrefixChar_076 -> 'L'
  PrefixChar_077 -> 'M'
  PrefixChar_078 -> 'N'
  PrefixChar_079 -> 'O'
  PrefixChar_080 -> 'P'
  PrefixChar_081 -> 'Q'
  PrefixChar_082 -> 'R'
  PrefixChar_083 -> 'S'
  PrefixChar_084 -> 'T'
  PrefixChar_085 -> 'U'
  PrefixChar_086 -> 'V'
  PrefixChar_087 -> 'W'
  PrefixChar_088 -> 'X'
  PrefixChar_089 -> 'Y'
  PrefixChar_090 -> 'Z'
  PrefixChar_091 -> '['
  PrefixChar_092 -> '\\'
  PrefixChar_093 -> ']'
  PrefixChar_094 -> '^'
  PrefixChar_095 -> '_'
  PrefixChar_096 -> '`'
  PrefixChar_097 -> 'a'
  PrefixChar_098 -> 'b'
  PrefixChar_099 -> 'c'
  PrefixChar_100 -> 'd'
  PrefixChar_101 -> 'e'
  PrefixChar_102 -> 'f'
  PrefixChar_103 -> 'g'
  PrefixChar_104 -> 'h'
  PrefixChar_105 -> 'i'
  PrefixChar_106 -> 'j'
  PrefixChar_107 -> 'k'
  PrefixChar_108 -> 'l'
  PrefixChar_109 -> 'm'
  PrefixChar_110 -> 'n'
  PrefixChar_111 -> 'o'
  PrefixChar_112 -> 'p'
  PrefixChar_113 -> 'q'
  PrefixChar_114 -> 'r'
  PrefixChar_115 -> 's'
  PrefixChar_116 -> 't'
  PrefixChar_117 -> 'u'
  PrefixChar_118 -> 'v'
  PrefixChar_119 -> 'w'
  PrefixChar_120 -> 'x'
  PrefixChar_121 -> 'y'
  PrefixChar_122 -> 'z'
  PrefixChar_123 -> '{'
  PrefixChar_124 -> '|'
  PrefixChar_125 -> '}'
  PrefixChar_126 -> '~'
