module Codec.Bech32.Suffix where

import Codec.Bech32.DataPart (DataPart)
import Codec.Bech32.Checksum (Checksum)

data Suffix = Suffix !DataPart !Checksum
