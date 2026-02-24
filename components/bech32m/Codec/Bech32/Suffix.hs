module Codec.Bech32.Suffix where

import Codec.Bech32.Suffix.Checksum (Checksum)
import Codec.Bech32.Suffix.Payload (Payload)
import Data.Text (Text)

data Suffix = Suffix
  { payload :: !Payload
  , checksum :: !Checksum
  }

data DecodeError = DecodeError

fromText :: Text -> Either DecodeError Suffix
fromText = undefined

getPayload :: Suffix -> Payload
getPayload (Suffix p _) = p

getChecksum :: Suffix -> Checksum
getChecksum (Suffix _ c) = c
