{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ViewPatterns #-}

module Codec.Bech32 where

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
import Data.Bits (Bits (shiftL, shiftR, testBit, xor, (.&.)), (.>>.))
import Data.Foldable qualified as Foldable
import Data.Functor ((<&>))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Word (Word32)
import Data.Word5 (Word5)
import Data.Word5 qualified as Word5
import Numeric.Natural (Natural)

separatorChar :: Char
separatorChar = '1'

humanReadablePartToWords :: Prefix -> [Word5]
humanReadablePartToWords (Prefix.toList -> cs) =
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
  let values = humanReadablePartToWords hrp <> Payload.toWord5List dp
      remainder = polymod values `xor` 1 -- XOR 1 for Bech32 final constant
  in Checksum
       (Word5.fromIntegral $ (remainder `shiftR` 25) .&. 0x1f)
       (Word5.fromIntegral $ (remainder `shiftR` 20) .&. 0x1f)
       (Word5.fromIntegral $ (remainder `shiftR` 15) .&. 0x1f)
       (Word5.fromIntegral $ (remainder `shiftR` 10) .&. 0x1f)
       (Word5.fromIntegral $ (remainder `shiftR` 5) .&. 0x1f)
       (Word5.fromIntegral $ remainder .&. 0x1f)

data DecodeError
  = MissingSeparator
  | PrefixTooShort
  | SuffixTooShort
  | InvalidChar !Natural
  | InvalidChecksum
  deriving (Eq, Ord, Show)

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
    splitOnSeparator =
      maybeToEither MissingSeparator . Text.splitOnLast separatorChar

    parsePrefix p =
      case Prefix.fromText p of
        Right prefix -> Right prefix
        Left Prefix.FromTextErrorEmpty -> Left PrefixTooShort
        Left (Prefix.FromTextErrorInvalidChar n) -> Left (InvalidChar n)

    parseSuffix s m =
      case Suffix.fromText s of
        Right suffix -> Right suffix
        Left Suffix.TooShort -> Left SuffixTooShort
        Left (Suffix.InvalidChar n) -> Left (InvalidChar (m + n + 1))

encode :: Prefix -> Payload -> Text
encode hrp dp =
  Prefix.toText hrp
    <> Text.singleton separatorChar
    <> Payload.toText dp
    <> Checksum.toText cs
  where
    cs = computeChecksum hrp dp
