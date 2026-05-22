{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ViewPatterns #-}

-- TODO:
-- Benchmark
-- Bech32 and Bech32m variants

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
import Data.Foldable qualified as Foldable
import Data.Functor ((<&>))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Word (Word32)
import Data.Word5 (Word5)
import Data.Word5 qualified as Word5
import Numeric.Natural (Natural)

encode :: Prefix -> Payload -> Text
encode prefix payload =
  Prefix.toText prefix
    <> Text.singleton separatorChar
    <> Payload.toText payload
    <> Checksum.toText cs
  where
    cs = computeChecksum prefix payload

decode :: Text -> Either DecodeError (Prefix, Payload)
decode t = do
  (prefixText, suffixText) <- splitOnSeparator t
  prefix <- parsePrefix prefixText
  suffix <- parseSuffix suffixText (fromIntegral (Text.length prefixText) + 1)
  let Suffix {payload, checksum} = suffix
  if computeChecksum prefix payload == checksum
    then Right (prefix, payload)
    else Left InvalidChecksum
  where
    splitOnSeparator :: Text -> Either DecodeError (Text, Text)
    splitOnSeparator =
      maybeToEither MissingSeparator . Text.splitOnLast separatorChar

    parsePrefix :: Text -> Either DecodeError Prefix
    parsePrefix p = first mapError $ Prefix.fromText p
      where
        mapError = \case
          Prefix.FromTextErrorEmpty -> PrefixTooShort
          Prefix.FromTextErrorInvalidChar n -> InvalidChar n

    parseSuffix :: Text -> Natural -> Either DecodeError Suffix
    parseSuffix s m = first mapError $ Suffix.fromText s
      where
        mapError = \case
          Suffix.TooShort -> SuffixTooShort
          Suffix.InvalidChar n -> InvalidChar (m + n + 1)

data DecodeError
  = MissingSeparator
  | PrefixTooShort
  | SuffixTooShort
  | InvalidChar !Natural
  | InvalidChecksum
  deriving (Eq, Ord, Show)

separatorChar :: Char
separatorChar = '1'

prefixToWord5List :: Prefix -> [Word5]
prefixToWord5List (Prefix.toList -> cs) =
  hiWords <> [0] <> loWords
  where
    hiWords = ordinals <&> Word5.fromIntegral . (.>>. 5)
    loWords = ordinals <&> Word5.fromIntegral
    ordinals = PrefixChar.toOrdinal <$> Foldable.toList cs

polymod :: [Word5] -> Word32
polymod = foldl' step 1
  where
    step :: Word32 -> Word5 -> Word32
    step c w =
      let c0 = fromIntegral (c `shiftR` 25) :: Word32
          c' =
            ((c .&. 0x1ffffff) `shiftL` 5)
              `xor` fromIntegral (Word5.toWord8 w)
          generators =
            [0x3b6a57b2, 0x26508e6d, 0x1ea119fa, 0x3d4233dd, 0x2a1462b3]
      in foldl
           (\acc (i, g) -> if testBit c0 i then acc `xor` g else acc)
           c'
           (zip [0 .. 4] generators)

computeChecksum :: Prefix -> Payload -> Checksum
computeChecksum hrp dp =
  let values = prefixToWord5List hrp <> Payload.toWord5List dp
      remainder = polymod values `xor` 1 -- XOR 1 for Bech32 final constant
  in Checksum
       (Word5.fromIntegral $ (remainder `shiftR` 25) .&. 0x1f)
       (Word5.fromIntegral $ (remainder `shiftR` 20) .&. 0x1f)
       (Word5.fromIntegral $ (remainder `shiftR` 15) .&. 0x1f)
       (Word5.fromIntegral $ (remainder `shiftR` 10) .&. 0x1f)
       (Word5.fromIntegral $ (remainder `shiftR` 5) .&. 0x1f)
       (Word5.fromIntegral $ remainder .&. 0x1f)
