module Codec.Bech32.Suffix where

import Codec.Bech32.Suffix.Payload (Payload)
import Codec.Bech32.Checksum (Checksum)

data Suffix = Suffix !Payload !Checksum
