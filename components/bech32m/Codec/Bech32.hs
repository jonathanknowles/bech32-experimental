{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Codec.Bech32 where

import Codec.Bech32.Checksum (Checksum (Checksum))
import Codec.Bech32.DataPart (DataPart)
import Codec.Bech32.DataPart qualified as DataPart
import Codec.Bech32.HumanReadableChar qualified as HumanReadableChar
import Codec.Bech32.HumanReadablePart (HumanReadablePart)
import Codec.Bech32.HumanReadablePart qualified as HumanReadablePart
import Data.Bits (Bits (shiftL, shiftR, testBit, xor, (.&.)), (.>>.))
import Data.Foldable qualified as Foldable
import Data.Functor ((<&>))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Word (Word32)
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

polymod :: [Word5] -> Word32
polymod = foldl step 1
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

computeChecksum :: HumanReadablePart -> DataPart -> Checksum
computeChecksum hrp dp =
  let values = humanReadablePartToWords hrp <> DataPart.toWord5List dp
      remainder = polymod values `xor` 1 -- XOR 1 for Bech32 final constant
  in Checksum
       (Word5.fromIntegral $ (remainder `shiftR` 25) .&. 0x1f)
       (Word5.fromIntegral $ (remainder `shiftR` 20) .&. 0x1f)
       (Word5.fromIntegral $ (remainder `shiftR` 15) .&. 0x1f)
       (Word5.fromIntegral $ (remainder `shiftR` 10) .&. 0x1f)
       (Word5.fromIntegral $ (remainder `shiftR` 5) .&. 0x1f)
       (Word5.fromIntegral $ remainder .&. 0x1f)
