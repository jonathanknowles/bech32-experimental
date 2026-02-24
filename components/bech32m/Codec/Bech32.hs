{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Codec.Bech32 where

import Codec.Bech32.Prefix (Prefix)
import Codec.Bech32.Prefix qualified as Prefix
import Codec.Bech32.Prefix.Char qualified as PrefixChar
import Codec.Bech32.Suffix.Checksum (Checksum (Checksum))
import Codec.Bech32.Suffix.Checksum qualified as Checksum
import Codec.Bech32.Suffix.Payload (Payload)
import Codec.Bech32.Suffix.Payload qualified as Payload
import Data.Bits (Bits (shiftL, shiftR, testBit, xor, (.&.)), (.>>.))
import Data.Foldable qualified as Foldable
import Data.Functor ((<&>))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Word (Word32)
import Data.Word5 (Word5)
import Data.Word5 qualified as Word5

-- TODO:
--
-- Payload should really be DataPayload (or similar) because the Payload
-- is actually the combination of the DataPayload and the Checksum.
--
-- Prefix
-- PrefixChar
--
-- DataPayload
-- Payload -- this can be a combination of DataPayload and Checksum
-- SuffixChar
--
--
-- How about:
-- Suffix
-- SuffixChar

separatorChar :: Char
separatorChar = '1'

splitOnSeparator :: Text -> Maybe (Text, Text)
splitOnSeparator t =
  case Text.breakOnEnd (Text.singleton separatorChar) t of
    ("", _) ->
      Nothing
    (prefixWith1, suffix) ->
      Just (Text.init prefixWith1, suffix)

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

encode :: Prefix -> Payload -> Text
encode hrp dp =
  Prefix.toText hrp
    <> "1"
    <> Payload.toText dp
    <> Checksum.toText cs
  where
    cs = computeChecksum hrp dp
