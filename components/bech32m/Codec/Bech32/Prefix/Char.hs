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
{-# LANGUAGE UndecidableInstances #-}

module Codec.Bech32.Prefix.Char
  ( -- * Type
    PrefixChar

    -- * Construction
  , fromChar
  , fromCharMaybe
  , fromOrdinal
  , fromOrdinalMaybe

    -- * Conversion
  , toChar
  , toOrdinal
  )
where

import Codec.Bech32.Prefix.Char.Types (KnownValidChar, KnownValidOrdinal)
import Data.Char qualified as Char
import Data.Finitary (Finitary)
import Data.Ix (Ix)
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy (Proxy))
import GHC.Generics (Generic)
import GHC.TypeLits
  ( charVal
  , natVal
  )
import Text.Read (Lexeme (Ident, Punc), Read (readPrec), lexP, parens, prec)
import Prelude

-- $setup
-- >>> :set -XDataKinds
-- >>> :set -XTypeApplications

--------------------------------------------------------------------------------
-- Type
--------------------------------------------------------------------------------

-- | A valid Bech32 prefix character.
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

--------------------------------------------------------------------------------
-- Construction from ordinary characters
--------------------------------------------------------------------------------

-- | Constructs a 'PrefixChar' from a type-level character.
--
-- >>> fromChar @'a'
-- fromChar @'a'
--
-- The character must be in one of the following inclusive intervals:
--
-- > ['!' .. '@']
-- > ['[' .. '~']
--
-- Applying this function to an invalid character will result in a type error:
--
-- >>> fromChar @'Z'
-- ...
-- ... Expected a character in one of the following inclusive intervals:
-- ...   ['!' .. '@']
-- ...   ['[' .. '~']
-- ...
fromChar :: forall c. KnownValidChar c => PrefixChar
fromChar =
  fromMaybe unexpectedOutOfRange $ fromCharMaybe $ charVal $ Proxy @c
  where
    unexpectedOutOfRange = error "PrefixChar.fromChar"

-- | Constructs a 'PrefixChar' from an ordinary character.
--
-- >>> fromCharMaybe 'a'
-- Just (fromChar @'a')
--
-- The character must be in one of the following inclusive intervals:
--
-- > ['!' .. '@']
-- > ['[' .. '~']
--
-- Applying this function to an invalid character will evaluate to 'Nothing':
--
-- >>> fromCharMaybe 'Z'
-- Nothing
fromCharMaybe :: Char -> Maybe PrefixChar
fromCharMaybe = fromOrdinalMaybe . Char.ord

unsafeFromChar :: Char -> PrefixChar
unsafeFromChar = fromMaybe onFailure . fromCharMaybe
  where
    onFailure = error "unsafeFromChar"

--------------------------------------------------------------------------------
-- Construction from ordinal numbers
--------------------------------------------------------------------------------

-- | Constructs a 'PrefixChar' from a type-level ordinal value.
--
-- >>> fromOrdinal @97
-- fromChar @'a'
--
-- The ordinal value must be in one of the following inclusive intervals:
--
-- > [33 ..  64]
-- > [91 .. 126]
--
-- Applying this function to an invalid ordinal will result in a type error:
--
-- >>> fromOrdinal @32
-- ...
-- ... Expected an ordinal in one of the following inclusive intervals:
-- ...   [33 ..  64]
-- ...   [91 .. 126]
-- ...
--
-- >>> fromOrdinal @127
-- ...
-- ... Expected an ordinal in one of the following inclusive intervals:
-- ...   [33 ..  64]
-- ...   [91 .. 126]
-- ...
fromOrdinal :: forall c. KnownValidOrdinal c => PrefixChar
fromOrdinal =
  fromMaybe unexpectedOutOfRange $ fromOrdinalMaybe $ natVal $ Proxy @c
  where
    unexpectedOutOfRange = error "PrefixChar.fromOrdinal"

-- | Constructs a 'PrefixChar' from an ordinal value.
--
-- >>> fromOrdinalMaybe 97
-- Just (fromChar @'a')
--
-- The ordinal value must be in one of the following inclusive intervals:
--
-- > [33 ..  64]
-- > [91 .. 126]
--
-- Applying this function to an invalid ordinal will evaluate to 'Nothing':
--
-- >>> fromOrdinalMaybe 32
-- Nothing
--
-- >>> fromOrdinalMaybe 127
-- Nothing
fromOrdinalMaybe :: Integral i => i -> Maybe PrefixChar
fromOrdinalMaybe = \case
  033 -> Just PrefixChar_033
  034 -> Just PrefixChar_034
  035 -> Just PrefixChar_035
  036 -> Just PrefixChar_036
  037 -> Just PrefixChar_037
  038 -> Just PrefixChar_038
  039 -> Just PrefixChar_039
  040 -> Just PrefixChar_040
  041 -> Just PrefixChar_041
  042 -> Just PrefixChar_042
  043 -> Just PrefixChar_043
  044 -> Just PrefixChar_044
  045 -> Just PrefixChar_045
  046 -> Just PrefixChar_046
  047 -> Just PrefixChar_047
  048 -> Just PrefixChar_048
  049 -> Just PrefixChar_049
  050 -> Just PrefixChar_050
  051 -> Just PrefixChar_051
  052 -> Just PrefixChar_052
  053 -> Just PrefixChar_053
  054 -> Just PrefixChar_054
  055 -> Just PrefixChar_055
  056 -> Just PrefixChar_056
  057 -> Just PrefixChar_057
  058 -> Just PrefixChar_058
  059 -> Just PrefixChar_059
  060 -> Just PrefixChar_060
  061 -> Just PrefixChar_061
  062 -> Just PrefixChar_062
  063 -> Just PrefixChar_063
  064 -> Just PrefixChar_064
  091 -> Just PrefixChar_091
  092 -> Just PrefixChar_092
  093 -> Just PrefixChar_093
  094 -> Just PrefixChar_094
  095 -> Just PrefixChar_095
  096 -> Just PrefixChar_096
  097 -> Just PrefixChar_097
  098 -> Just PrefixChar_098
  099 -> Just PrefixChar_099
  100 -> Just PrefixChar_100
  101 -> Just PrefixChar_101
  102 -> Just PrefixChar_102
  103 -> Just PrefixChar_103
  104 -> Just PrefixChar_104
  105 -> Just PrefixChar_105
  106 -> Just PrefixChar_106
  107 -> Just PrefixChar_107
  108 -> Just PrefixChar_108
  109 -> Just PrefixChar_109
  110 -> Just PrefixChar_110
  111 -> Just PrefixChar_111
  112 -> Just PrefixChar_112
  113 -> Just PrefixChar_113
  114 -> Just PrefixChar_114
  115 -> Just PrefixChar_115
  116 -> Just PrefixChar_116
  117 -> Just PrefixChar_117
  118 -> Just PrefixChar_118
  119 -> Just PrefixChar_119
  120 -> Just PrefixChar_120
  121 -> Just PrefixChar_121
  122 -> Just PrefixChar_122
  123 -> Just PrefixChar_123
  124 -> Just PrefixChar_124
  125 -> Just PrefixChar_125
  126 -> Just PrefixChar_126
  ___ -> Nothing

--------------------------------------------------------------------------------
-- Conversion to ordinary characters
--------------------------------------------------------------------------------

-- | Converts a 'PrefixChar' to an ordinary character.
--
-- >>> toChar (fromChar @'a')
-- 'a'
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

--------------------------------------------------------------------------------
-- Conversion to ordinal numbers
--------------------------------------------------------------------------------

-- | Converts a 'PrefixChar' to an ordinal number.
--
-- >>> toOrdinal (fromOrdinal @97)
-- 97
toOrdinal :: PrefixChar -> Int
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
