{-# LANGUAGE NamedFieldPuns #-}

module Codec.Bech32.Suffix where

import Codec.Bech32.Suffix.Char qualified as SuffixChar
import Codec.Bech32.Suffix.Checksum (Checksum (..))
import Codec.Bech32.Suffix.Payload (Payload)
import Codec.Bech32.Suffix.Payload qualified as Payload
import Codec.Bech32.Utilities (maybeToEither)
import Data.List qualified as List
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Word5 (Word5)
import Numeric.Natural (Natural)

data Suffix = Suffix
  { payload :: !Payload
  , checksum :: !Checksum
  }

data DecodeError
  = TooShort
  | InvalidChar !Natural

fromText :: Text -> Either DecodeError Suffix
fromText t = do
  ws <- parseToWords t
  case List.reverse ws of
    (c5 : c4 : c3 : c2 : c1 : c0 : cs) ->
      Right $
        Suffix
          (Payload.fromWord5List cs)
          (Checksum {c0, c1, c2, c3, c4, c5})
    _ ->
      Left TooShort
  where
    parseToWords :: Text -> Either DecodeError [Word5]
    parseToWords = traverse parseChar . zip [0 ..] . Text.unpack
      where
        parseChar (n, c) =
          maybeToEither
            (InvalidChar n)
            (SuffixChar.toWord5 <$> SuffixChar.fromCharMaybe c)

getPayload :: Suffix -> Payload
getPayload (Suffix p _) = p

getChecksum :: Suffix -> Checksum
getChecksum (Suffix _ c) = c
