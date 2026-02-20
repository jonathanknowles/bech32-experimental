{-# LANGUAGE OverloadedStrings #-}

module Codec.Bech32 where

import Data.Text (Text)
import Data.Text qualified as Text

separatorChar :: Char
separatorChar = '1'

splitOnSeparator :: Text -> Maybe (Text, Text)
splitOnSeparator t =
  case Text.breakOnEnd (Text.singleton separatorChar) t of
    ("", _) ->
      Nothing
    (prefixWith1, suffix) ->
      Just (Text.init prefixWith1, suffix)
