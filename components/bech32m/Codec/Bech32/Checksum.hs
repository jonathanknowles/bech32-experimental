{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NamedFieldPuns #-}

module Codec.Bech32.Checksum where

import Codec.Bech32.Suffix.Char qualified as SuffixChar
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Word5 (Word5)

data Checksum = Checksum
  { c0 :: !Word5
  , c1 :: !Word5
  , c2 :: !Word5
  , c3 :: !Word5
  , c4 :: !Word5
  , c5 :: !Word5
  }
  deriving stock (Eq, Ord, Read, Show)

toText :: Checksum -> Text
toText Checksum {c0, c1, c2, c3, c4, c5} =
  Text.pack $ word5ToChar <$> [c0, c1, c2, c3, c4, c5]
  where
    word5ToChar :: Word5 -> Char
    word5ToChar = SuffixChar.toChar . SuffixChar.fromWord5
