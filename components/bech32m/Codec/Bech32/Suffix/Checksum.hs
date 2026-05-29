{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NamedFieldPuns #-}

module Codec.Bech32.Suffix.Checksum
  ( Checksum (..)
  , length
  , fromText
  , toText
  , toWord5List
  )
where

import Codec.Bech32.Suffix.Char qualified as SuffixChar
import Codec.Bech32.Utilities (maybeToEither)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Word5 (Word5)
import GHC.Natural (Natural)
import Prelude hiding (length)

data Checksum = Checksum
  { c0 :: !Word5
  , c1 :: !Word5
  , c2 :: !Word5
  , c3 :: !Word5
  , c4 :: !Word5
  , c5 :: !Word5
  }
  deriving stock (Eq, Ord, Read, Show)

data DecodeError
  = InvalidLength
  | InvalidChar !Natural
  deriving (Eq, Ord, Show)

length :: Checksum -> Int
length = const 6

toWord5List :: Checksum -> [Word5]
toWord5List Checksum {c0, c1, c2, c3, c4, c5} = [c0, c1, c2, c3, c4, c5]

fromText :: Text -> Either DecodeError Checksum
fromText text = do
  ws <- parseToWords text
  case ws of
    [c0, c1, c2, c3, c4, c5] -> Right Checksum {c0, c1, c2, c3, c4, c5}
    _ -> Left InvalidLength
  where
    parseToWords :: Text -> Either DecodeError [Word5]
    parseToWords = traverse parseChar . zip [0 ..] . Text.unpack
      where
        parseChar (n, c) =
          maybeToEither
            (InvalidChar n)
            (SuffixChar.toWord5 <$> SuffixChar.fromCharMaybe c)

toText :: Checksum -> Text
toText Checksum {c0, c1, c2, c3, c4, c5} =
  Text.pack $ word5ToChar <$> [c0, c1, c2, c3, c4, c5]
  where
    word5ToChar :: Word5 -> Char
    word5ToChar = SuffixChar.toChar . SuffixChar.fromWord5
