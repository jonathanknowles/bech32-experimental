module Codec.Bech32.Suffix where

import Codec.Bech32.Suffix.Checksum (Checksum)
import Codec.Bech32.Suffix.Payload (Payload)

data Suffix = Suffix !Payload !Checksum
