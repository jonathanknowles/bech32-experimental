{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Codec.Bech32 where

import Codec.Bech32.HumanReadableChar qualified as HumanReadableChar
import Codec.Bech32.HumanReadablePart (HumanReadablePart)
import Codec.Bech32.HumanReadablePart qualified as HumanReadablePart
import Data.Bits ((.>>.))
import Data.Foldable qualified as Foldable
import Data.Functor ((<&>))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Word5 (Word5)
import Data.Word5 qualified as Word5

separatorChar :: Char
separatorChar = '1'

splitOnSeparator :: Text -> Maybe (Text, Text)
splitOnSeparator t =
  case Text.breakOnEnd (Text.singleton separatorChar) t of
    ("", _) ->
      Nothing
    (prefixWith1, suffix) ->
      Just (Text.init prefixWith1, suffix)

humanReadablePartToWords :: HumanReadablePart -> [Word5]
humanReadablePartToWords (HumanReadablePart.toList -> cs) =
  hiWords <> [0] <> loWords
  where
    hiWords = ordinals <&> Word5.fromIntegral . (.>>. 5)
    loWords = ordinals <&> Word5.fromIntegral
    ordinals = HumanReadableChar.toOrdinal <$> Foldable.toList cs
