{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ViewPatterns #-}

{- HLINT ignore "Redundant bracket" -}

-- TODO:
--
-- Benchmarks
--
-- Bech32 and Bech32m variants
--
-- Case handling:
-- BIP-173 specifies that implementations should produce lowercase, but
-- decoders must accept either all-lowercase or all-uppercase (never mixed).
-- Mixed case is explicitly invalid.

module Codec.Bech32
  ( encode
  , decode
  , DecodeError (..)
  )
where

import Codec.Bech32.Prefix (Prefix)
import Codec.Bech32.Prefix qualified as Prefix
import Codec.Bech32.Prefix.Char qualified as PrefixChar
import Codec.Bech32.Suffix (Suffix (Suffix, checksum, payload))
import Codec.Bech32.Suffix qualified as Suffix
import Codec.Bech32.Suffix.Checksum (Checksum (Checksum))
import Codec.Bech32.Suffix.Checksum qualified as Checksum
import Codec.Bech32.Suffix.Payload (Payload)
import Codec.Bech32.Suffix.Payload qualified as Payload
import Codec.Bech32.Utilities (maybeToEither)
import Codec.Bech32.Utilities qualified as Text (splitOnLast)
import Data.Bifunctor (Bifunctor (first))
import Data.Bits (Bits (shiftL, shiftR, testBit, xor, (.&.)), (.>>.))
import Data.Char qualified as Char
import Data.Foldable qualified as Foldable
import Data.Functor ((<&>))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Word (Word32)
import Data.Word5 (Word5)
import Data.Word5 qualified as Word5

encode :: Prefix -> Payload -> Text
encode prefix payload =
  Text.concat
    [ Prefix.toText prefix
    , Text.singleton separatorChar
    , Payload.toText payload
    , Checksum.toText (computeChecksum prefix payload)
    ]

decode :: Text -> Either DecodeError (Prefix, Payload)
decode text = do
  (prefixText, suffixText) <- splitOnSeparator =<< normaliseCase text
  prefix <- parsePrefix prefixText
  suffix <- parseSuffix suffixText prefix
  extractPayload prefix suffix
  where
    normaliseCase :: Text -> Either DecodeError Text
    normaliseCase t =
      if hasUpper
        then (if hasLower then Left MixedCase else Right (Text.toLower t))
        else Right t
      where
        hasUpper = Text.any Char.isUpper t
        hasLower = Text.any Char.isLower t

    splitOnSeparator :: Text -> Either DecodeError (Text, Text)
    splitOnSeparator =
      maybeToEither MissingSeparator . Text.splitOnLast separatorChar

    parsePrefix :: Text -> Either DecodeError Prefix
    parsePrefix prefixText = first mapError $ Prefix.fromText prefixText
      where
        mapError = \case
          Prefix.FromTextErrorEmpty -> PrefixTooShort
          Prefix.FromTextErrorInvalidChar n -> InvalidChar n

    parseSuffix :: Text -> Prefix -> Either DecodeError Suffix
    parseSuffix suffixText prefix = first mapError $ Suffix.fromText suffixText
      where
        mapError = \case
          Suffix.TooShort -> SuffixTooShort
          Suffix.InvalidChar j -> InvalidChar (Prefix.length prefix + j + 1)

    extractPayload :: Prefix -> Suffix -> Either DecodeError (Prefix, Payload)
    extractPayload prefix Suffix {payload, checksum}
      | computeChecksum prefix payload == checksum =
          Right (prefix, payload)
      | otherwise =
          Left InvalidChecksum

data DecodeError
  = MissingSeparator
  | MixedCase
  | PrefixTooShort
  | SuffixTooShort
  | InvalidChar !Int
  | InvalidChecksum
  deriving (Eq, Ord, Show)

separatorChar :: Char
separatorChar = '1'

computeChecksum :: Prefix -> Payload -> Checksum
computeChecksum prefix payload =
  Checksum
    (Word5.fromIntegral $ (remainder `shiftR` 25) .&. 0x1f)
    (Word5.fromIntegral $ (remainder `shiftR` 20) .&. 0x1f)
    (Word5.fromIntegral $ (remainder `shiftR` 15) .&. 0x1f)
    (Word5.fromIntegral $ (remainder `shiftR` 10) .&. 0x1f)
    (Word5.fromIntegral $ (remainder `shiftR` 05) .&. 0x1f)
    (Word5.fromIntegral $ (remainder {---------}) .&. 0x1f)
  where
    remainder = polymod (values <> replicate 6 0) `xor` 1
    values = prefixToWord5List prefix <> Payload.toWord5List payload

polymod :: [Word5] -> Word32
polymod = foldl' step 1
  where
    generators :: [Word32]
    generators = [0x3b6a57b2, 0x26508e6d, 0x1ea119fa, 0x3d4233dd, 0x2a1462b3]

    step :: Word32 -> Word5 -> Word32
    step c w =
      foldl
        (\acc (i, g) -> if testBit c0 i then acc `xor` g else acc)
        c'
        (zip [0 .. 4] generators)
      where
        c0 :: Word32
        c0 = fromIntegral (c `shiftR` 25)

        c' :: Word32
        c' = ((c .&. 0x1ffffff) `shiftL` 5) `xor` fromIntegral (Word5.toWord8 w)

prefixToWord5List :: Prefix -> [Word5]
prefixToWord5List (Prefix.toList -> cs) =
  hiWords <> [0] <> loWords
  where
    hiWords = ordinals <&> Word5.fromIntegral . (.>>. 5)
    loWords = ordinals <&> Word5.fromIntegral
    ordinals = PrefixChar.toOrdinal <$> Foldable.toList cs
