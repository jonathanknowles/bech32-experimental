module Codec.Bech32 where

import Data.Text (Text)
import Data.Text qualified as Text

separatorChar :: Char
separatorChar = '1'

splitOnSeparator :: Text -> Maybe (Text, Text)
splitOnSeparator t = undefined
  where
    foo = Text.reverse t
