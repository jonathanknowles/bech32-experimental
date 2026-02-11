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

module Data.Bech32.HumanReadableChar
  ( HumanReadableChar
  , fromChar
  , fromCharMaybe
  , toChar
  , minBound
  , maxBound
  )
where

import Data.Kind (Constraint)
import Data.Proxy (Proxy (Proxy))
import Data.Type.Bool (Not, type (&&))
import Data.Type.Equality (type (==))
import GHC.TypeError (Assert, TypeError)
import GHC.TypeError qualified as TypeError
import GHC.TypeLits (CmpChar, KnownChar, charVal)
import Prelude hiding (maxBound, minBound)
import GHC.Generics (Generic)
import Data.Ix (Ix)
import Data.Finitary (Finitary)

data HumanReadableChar where
  HumanReadableChar :: IsValidChar c => Proxy c -> HumanReadableChar

data HRC
  = HRC_033
  | HRC_034
  | HRC_035
  | HRC_036
  | HRC_037
  | HRC_038
  | HRC_039
  | HRC_040
  | HRC_041
  | HRC_042
  | HRC_043
  | HRC_044
  | HRC_045
  | HRC_046
  | HRC_047
  | HRC_048
  | HRC_049
  | HRC_050
  | HRC_051
  | HRC_052
  | HRC_053
  | HRC_054
  | HRC_055
  | HRC_056
  | HRC_057
  | HRC_058
  | HRC_059
  | HRC_060
  | HRC_061
  | HRC_062
  | HRC_063
  | HRC_064
  | HRC_065
  | HRC_066
  | HRC_067
  | HRC_068
  | HRC_069
  | HRC_070
  | HRC_071
  | HRC_072
  | HRC_073
  | HRC_074
  | HRC_075
  | HRC_076
  | HRC_077
  | HRC_078
  | HRC_079
  | HRC_080
  | HRC_081
  | HRC_082
  | HRC_083
  | HRC_084
  | HRC_085
  | HRC_086
  | HRC_087
  | HRC_088
  | HRC_089
  | HRC_090
  | HRC_091
  | HRC_092
  | HRC_093
  | HRC_094
  | HRC_095
  | HRC_096
  | HRC_097
  | HRC_098
  | HRC_099
  | HRC_100
  | HRC_101
  | HRC_102
  | HRC_103
  | HRC_104
  | HRC_105
  | HRC_106
  | HRC_107
  | HRC_108
  | HRC_109
  | HRC_110
  | HRC_111
  | HRC_112
  | HRC_113
  | HRC_114
  | HRC_115
  | HRC_116
  | HRC_117
  | HRC_118
  | HRC_119
  | HRC_120
  | HRC_121
  | HRC_122
  | HRC_123
  | HRC_124
  | HRC_125
  | HRC_126
  deriving stock (Bounded, Enum, Eq, Generic, Ix, Ord, Read, Show)
  deriving anyclass Finitary

instance Show HumanReadableChar where
  showsPrec _ hrc =
    showString "fromChar @" . shows (toChar hrc)

type family IsValidChar (c :: Char) :: Constraint where
  IsValidChar c =
    ( KnownChar c
    , Assert (CharInRange c) (TypeError CharError)
    )

type CharInRange c =
  (&&)
    (Not (CmpChar c '!' == 'LT))
    (Not (CmpChar c '~' == 'GT))

type CharError =
  TypeError.Text
    "A HumanReadableChar must be a character in the range ['!' .. '~']."

fromChar :: forall c. IsValidChar c => HumanReadableChar
fromChar = HumanReadableChar $ Proxy @c

minBound :: HumanReadableChar
minBound = fromChar @'!'

maxBound :: HumanReadableChar
maxBound = fromChar @'~'

fromCharMaybe :: Char -> Maybe HumanReadableChar
fromCharMaybe = \case
  -- Characters that require escaping:
  '\'' -> char @'\''
  '\\' -> char @'\\'
  -- Characters that don't requiring escaping:
  '!' -> char @'!'
  '"' -> char @'"'
  '#' -> char @'#'
  '$' -> char @'$'
  '%' -> char @'%'
  '&' -> char @'&'
  '(' -> char @'('
  ')' -> char @')'
  '*' -> char @'*'
  '+' -> char @'+'
  ',' -> char @','
  '-' -> char @'-'
  '.' -> char @'.'
  '/' -> char @'/'
  '0' -> char @'0'
  '1' -> char @'1'
  '2' -> char @'2'
  '3' -> char @'3'
  '4' -> char @'4'
  '5' -> char @'5'
  '6' -> char @'6'
  '7' -> char @'7'
  '8' -> char @'8'
  '9' -> char @'9'
  ':' -> char @':'
  ';' -> char @';'
  '<' -> char @'<'
  '=' -> char @'='
  '>' -> char @'>'
  '?' -> char @'?'
  '@' -> char @'@'
  'A' -> char @'A'
  'B' -> char @'B'
  'C' -> char @'C'
  'D' -> char @'D'
  'E' -> char @'E'
  'F' -> char @'F'
  'G' -> char @'G'
  'H' -> char @'H'
  'I' -> char @'I'
  'J' -> char @'J'
  'K' -> char @'K'
  'L' -> char @'L'
  'M' -> char @'M'
  'N' -> char @'N'
  'O' -> char @'O'
  'P' -> char @'P'
  'Q' -> char @'Q'
  'R' -> char @'R'
  'S' -> char @'S'
  'T' -> char @'T'
  'U' -> char @'U'
  'V' -> char @'V'
  'W' -> char @'W'
  'X' -> char @'X'
  'Y' -> char @'Y'
  'Z' -> char @'Z'
  '[' -> char @'['
  ']' -> char @']'
  '^' -> char @'^'
  '_' -> char @'_'
  '`' -> char @'`'
  'a' -> char @'a'
  'b' -> char @'b'
  'c' -> char @'c'
  'd' -> char @'d'
  'e' -> char @'e'
  'f' -> char @'f'
  'g' -> char @'g'
  'h' -> char @'h'
  'i' -> char @'i'
  'j' -> char @'j'
  'k' -> char @'k'
  'l' -> char @'l'
  'm' -> char @'m'
  'n' -> char @'n'
  'o' -> char @'o'
  'p' -> char @'p'
  'q' -> char @'q'
  'r' -> char @'r'
  's' -> char @'s'
  't' -> char @'t'
  'u' -> char @'u'
  'v' -> char @'v'
  'w' -> char @'w'
  'x' -> char @'x'
  'y' -> char @'y'
  'z' -> char @'z'
  '{' -> char @'{'
  '|' -> char @'|'
  '}' -> char @'}'
  '~' -> char @'~'
  ___ -> Nothing
  where
    char :: forall c. IsValidChar c => Maybe HumanReadableChar
    char = Just (fromChar @c)

toChar :: HumanReadableChar -> Char
toChar (HumanReadableChar (p :: Proxy c)) = charVal p
