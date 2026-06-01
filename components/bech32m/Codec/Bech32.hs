{-# LANGUAGE DuplicateRecordFields #-}
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
-- Error detection / correction.
-- Use "code point" instead of ordinal language (including in this module).
-- Remove ordinals code for prefix chars, or add it to suffixes too.

module Codec.Bech32
  ( encode
  , decode
  , EncodingVariant (..)
  , DecodingResult (..)
  , DecodingError (..)
  , DecodingErrorDiagnosis (..)
  , DecodingErrorLocations (..)
  , DecodingErrorCorrection (..)
  )
where

import Codec.Bech32.Prefix (Prefix)
import Codec.Bech32.Prefix qualified as Prefix
import Codec.Bech32.Prefix.Char qualified as PrefixChar
import Codec.Bech32.Suffix (Suffix (Suffix, payload))
import Codec.Bech32.Suffix qualified as Suffix
import Codec.Bech32.Suffix.Char qualified as SuffixChar
import Codec.Bech32.Suffix.Checksum (Checksum (Checksum))
import Codec.Bech32.Suffix.Checksum qualified as Checksum
import Codec.Bech32.Suffix.Payload (Payload)
import Codec.Bech32.Suffix.Payload qualified as Payload
import Codec.Bech32.Utilities (maybeToEither)
import Codec.Bech32.Utilities qualified as Text (splitOnLast)
import Data.Array (Array, listArray, (!))
import Data.Bifunctor (Bifunctor (first))
import Data.Bits (Bits (shiftL, shiftR, testBit, xor, (.&.)), (.>>.))
import Data.Char qualified as Char
import Data.Foldable qualified as Foldable
import Data.Functor ((<&>))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Word (Word32, Word8)
import Data.Word5 (Word5)
import Data.Word5 qualified as Word5

--------------------------------------------------------------------------------
-- Public API
--------------------------------------------------------------------------------

encode :: Prefix -> Payload -> Text
encode prefix payload =
  Text.concat
    [ Prefix.toText prefix
    , Text.singleton separatorChar
    , Payload.toText payload
    , Checksum.toText (computeChecksum prefix payload)
    ]

decode :: Text -> Either DecodingError DecodingResult
decode text = do
  normalised <- normaliseCase text
  (prefixText, suffixText) <- splitOnSeparator normalised
  prefix <- parsePrefix prefixText
  suffix <- parseSuffix suffixText prefix
  verifyAndDiagnose prefix suffix
  where
    normaliseCase :: Text -> Either DecodingError Text
    normaliseCase t =
      if hasUpper
        then (if hasLower then Left MixedCase else Right (Text.toLower t))
        else Right t
      where
        hasUpper = Text.any Char.isUpper t
        hasLower = Text.any Char.isLower t

    splitOnSeparator :: Text -> Either DecodingError (Text, Text)
    splitOnSeparator =
      maybeToEither MissingSeparator . Text.splitOnLast separatorChar

    parsePrefix :: Text -> Either DecodingError Prefix
    parsePrefix prefixText = first mapError $ Prefix.fromText prefixText
      where
        mapError = \case
          Prefix.FromTextErrorEmpty -> PrefixTooShort
          Prefix.FromTextErrorInvalidChar n -> InvalidChar n

    parseSuffix :: Text -> Prefix -> Either DecodingError Suffix
    parseSuffix suffixText prefix = first mapError $ Suffix.fromText suffixText
      where
        mapError = \case
          Suffix.TooShort -> SuffixTooShort
          Suffix.InvalidChar j -> InvalidChar (Prefix.length prefix + j + 1)

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

-- | Distinguishes between the two Bech32 checksum variants.
--
-- 'Bech32' is defined in BIP-173 and uses the constant @0x00000001@.
-- 'Bech32m' is defined in BIP-350 and uses the constant @0x2bc830a3@.
data EncodingVariant
  = Bech32
  | Bech32m
  deriving (Eq, Ord, Show)

-- | The result of a successful 'decode'.
data DecodingResult = DecodingResult
  { variant :: !EncodingVariant
  , prefix :: !Prefix
  , payload :: !Payload
  }
  deriving (Eq, Ord, Show)

-- | Describes why a 'decode' failed.
--
-- All positions are 0-based indices into the original input 'Text'.
data DecodingError
  = MissingSeparator
  | MixedCase
  | PrefixTooShort
  | SuffixTooShort
  | InvalidChar !Int -- shoud this give all the positions, and not just one?
  | InvalidChecksum !DecodingErrorDiagnosis
  deriving (Eq, Ord, Show)

-- | Diagnostic information extracted from a failed checksum.
--
-- The BCH code underlying Bech32/Bech32m has a minimum Hamming distance of 6
-- over GF(32), which guarantees detection of all error patterns of weight 1–5.
-- Of these, patterns of weight 1–2 can additionally be /located/ and
-- /corrected/ by the decoder.
data DecodingErrorDiagnosis
  = -- | The syndrome is consistent with a weight-1 or weight-2 error pattern.
    --   The error positions have been exactly identified and the correct
    --   characters computed, modulo the ~2^{-30} probability of an undetected
    --   weight-6+ pattern masquerading as a low-weight one.
    WithinCorrectionCapacity !DecodingErrorLocations
  | -- | The syndrome is non-zero (errors are therefore certain) but the error
    --   pattern is of weight 3–5, which is beyond the code's correction
    --   capacity. The positions of the errors cannot be determined.
    WithinDetectionCapacity
  deriving (Eq, Ord, Show)

-- | One or two located and corrected characters.
--
-- When 'TwoErrors' is returned, the first 'DecodingErrorCorrection' has the
-- smaller 'position' (i.e. the errors are presented in left-to-right reading
-- order).
data DecodingErrorLocations
  = OneError !DecodingErrorCorrection
  | TwoErrors !DecodingErrorCorrection !DecodingErrorCorrection
  deriving (Eq, Ord, Show)

-- | A single located error with its corrected character value.
data DecodingErrorCorrection = DecodingErrorCorrection
  { position :: !Int
  -- ^ 0-based index into the original input 'Text'.
  , corrected :: !Char
  -- ^ The character that should appear at 'position'.
  }
  deriving (Eq, Ord, Show)

--------------------------------------------------------------------------------
-- Checksum verification and error diagnosis
--------------------------------------------------------------------------------

verifyAndDiagnose :: Prefix -> Suffix -> Either DecodingError DecodingResult
verifyAndDiagnose prefix suffix@Suffix {payload} =
  case polymodResult of
    r
      | r == bech32Const ->
          Right DecodingResult {prefix, payload, variant = Bech32}
      | r == bech32mConst ->
          Right DecodingResult {prefix, payload, variant = Bech32m}
      | otherwise ->
          Left $
            InvalidChecksum $
              diagnoseSyndrome
                (Prefix.length prefix)
                (Suffix.length suffix)
                suffixWords
                r
  where
    suffixWords = Suffix.toWord5List suffix
    polymodResult = polymod (prefixToWord5List prefix <> suffixWords)

--------------------------------------------------------------------------------
-- Error diagnosis
--------------------------------------------------------------------------------

-- | Given a failed polymod result, attempt to locate up to two errors within
-- the suffix (payload + checksum characters).
--
-- We try both the Bech32 and Bech32m residues and return the diagnosis from
-- whichever yields the sharper localisation.
diagnoseSyndrome
  :: Int
  -- ^ Prefix length (in characters).
  -> Int
  -- ^ Data length = payload length + 6 checksum characters.
  -> [Word5]
  -- ^ Data word5 values: payload words followed by checksum words.
  -> Word32
  -- ^ Raw polymod result (before XOR with any variant constant).
  -> DecodingErrorDiagnosis
diagnoseSyndrome prefixLength suffixLength dataWords polymodResult =
  case pickBest bech32Errors bech32mErrors of
    [] -> WithinDetectionCapacity
    [(p, mag)] ->
      WithinCorrectionCapacity $
        OneError (makeDecodingErrorCorrection p mag)
    [(p1, m1), (p2, m2)] ->
      WithinCorrectionCapacity $
        -- p1 < p2 by contract from locateErrors;
        -- p2 has the smaller string position (further
        -- left), so it is presented first.
        TwoErrors
          (makeDecodingErrorCorrection p2 m2)
          (makeDecodingErrorCorrection p1 m1)
    _ -> WithinDetectionCapacity -- unreachable
  where
    bech32Errors = locateErrors (polymodResult `xor` bech32Const) suffixLength
    bech32mErrors = locateErrors (polymodResult `xor` bech32mConst) suffixLength

    -- Prefer the result with fewer located errors; ties go to Bech32.
    pickBest :: [(Int, Word5)] -> [(Int, Word5)] -> [(Int, Word5)]
    pickBest [] bs = bs
    pickBest as [] = as
    pickBest as bs
      | length as <= length bs = as
      | otherwise = bs

    -- Convert a (data-position-from-end, error-magnitude) pair into a
    -- DecodingErrorCorrection in string coordinates.
    --
    -- Data position p is 0-indexed from the END of the data array:
    --   p = 0                → last character of the checksum
    --   p = suffixLength - 1 → first character of the payload
    --
    -- String index = prefixLength + 1 (separator) + (suffixLength - 1 - p)
    makeDecodingErrorCorrection :: Int -> Word5 -> DecodingErrorCorrection
    makeDecodingErrorCorrection p mag =
      let dataIdx = suffixLength - 1 - p
          stringIdx = prefixLength + 1 + dataIdx
          received = dataWords !! dataIdx
          corrected_ = xorWord5 received mag
      in DecodingErrorCorrection
           { position = stringIdx
           , corrected = SuffixChar.toChar (SuffixChar.fromWord5 corrected_)
           }

xorWord5 :: Word5 -> Word5 -> Word5
xorWord5 a b =
  Word5.fromIntegral (Word5.toWord8 a `xor` Word5.toWord8 b :: Word8)

--------------------------------------------------------------------------------
-- Error location via BCH syndrome analysis
--
-- The Bech32/Bech32m BCH code has minimum distance 6 over GF(32), giving
-- guaranteed detection of all weight-1..5 error patterns and guaranteed
-- correction of all weight-1..2 patterns.
--
-- Error location follows the algorithm from sipa's reference implementation
-- (https://github.com/sipa/bech32/blob/master/ecc/javascript/bech32_ecc.js).
-- The 30-bit polymod residue is mapped into three elements of GF(1024) =
-- GF(2^10) via 'syndromeFromResidue'. Newton's identity checks then determine
-- whether a weight-1 or weight-2 solution exists, and Chien-search-style
-- iteration locates the error positions. Error magnitudes are recovered by
-- observing that GF(32) embeds in GF(1024) as the unique subfield of
-- order 32, whose non-zero elements are exactly those with GF(1024) discrete
-- log divisible by 33 (since 1023 / 31 = 33).
--------------------------------------------------------------------------------

-- | Attempt to locate errors within the data portion of a Bech32/Bech32m
-- string, given a non-zero residue (i.e. @polymod result XOR variant
-- constant@).
--
-- Returns a list of @(dataPositionFromEnd, errorMagnitude)@ pairs, sorted by
-- data position ascending (i.e. the pair with the smaller position, which
-- corresponds to the character furthest right in the string, comes first).
--
-- Returns @[]@ if no 1- or 2-error pattern is consistent with the syndrome.
locateErrors :: Word32 -> Int -> [(Int, Word5)]
locateErrors residue len
  | residue == 0 = []
  | otherwise =
      let syn = syndromeFromResidue residue
          s0 = fromIntegral (syn .&. 0x3ff) :: Int
          s1 = fromIntegral ((syn `shiftR` 10) .&. 0x3ff) :: Int
          s2 = fromIntegral (syn `shiftR` 20) :: Int
          ls0 = gf1024LogArr ! s0
          ls1 = gf1024LogArr ! s1
          ls2 = gf1024LogArr ! s2
      in tryOneError s0 s1 s2 ls0 ls1 ls2 len
           `orElse` tryTwoErrors s0 s1 s2 ls0 ls1 len
  where
    orElse [] ys = ys
    orElse xs _ = xs

-- | Check whether the syndrome is consistent with a single error.
tryOneError
  :: Int
  -> Int
  -> Int
  -- ^ s0, s1, s2 (elements of GF(1024))
  -> Int
  -> Int
  -> Int
  -- ^ ls0, ls1, ls2 (their discrete logs, -1 if zero)
  -> Int
  -- ^ data length
  -> [(Int, Word5)]
tryOneError _s0 _s1 _s2 ls0 ls1 ls2 len
  | ls0 /= (-1)
  , ls1 /= (-1)
  , ls2 /= (-1)
  , (2 * ls1 - ls2 - ls0 + 2046) `mod` 1023 == 0 =
      let p1 = (ls1 - ls0 + 1023) `mod` 1023
          l_e1 = ls0 + (1023 - 997) * p1
      in ([(p1, magnitudeFromLog l_e1) | not (p1 >= len || l_e1 `mod` 33 /= 0)])
  | otherwise = []

tryTwoErrors :: Int -> Int -> Int -> Int -> Int -> Int -> [(Int, Word5)]
tryTwoErrors s0 s1 s2 ls0 ls1 len = go 0
  where
    go p1
      | p1 >= len = []
      | s2_s1p1 == 0 || s1_s0p1 == 0 = go (p1 + 1)
      | p2 >= len || p1 == p2 = go (p1 + 1)
      | s1_s0p2 == 0 = go (p1 + 1)
      | l_e1 `mod` 33 /= 0
          || l_e2 `mod` 33 /= 0 =
          go (p1 + 1)
      | p1 < p2 =
          [(p1, magnitudeFromLog l_e1), (p2, magnitudeFromLog l_e2)]
      | otherwise =
          [(p2, magnitudeFromLog l_e2), (p1, magnitudeFromLog l_e1)]
      where
        inv_p1_p2 = 1023 - gf1024LogArr ! (gf1024ExpArr ! p1 `xor` gf1024ExpArr ! p2)
        l_e1 = gf1024LogArr ! s1_s0p2 + inv_p1_p2 + (1023 - 997) * p1
        l_e2 = l_s1_s0p1 + inv_p1_p2 + (1023 - 997) * p2
        l_s1_s0p1 = gf1024LogArr ! s1_s0p1
        p2 = (gf1024LogArr ! s2_s1p1 - l_s1_s0p1 + 1023) `mod` 1023
        s1_s0p1 = s1 `xor` mulPow s0 ls0 p1
        s1_s0p2 = s1 `xor` mulPow s0 ls0 p2
        s2_s1p1 = s2 `xor` mulPow s1 ls1 p1

    -- Multiply a GF(1024) element x by α^p,
    -- given x itself and ls, its precomputed discrete log.
    mulPow :: Int -> Int -> Int -> Int
    mulPow 0 _ _ = 0
    mulPow _ ls p = gf1024ExpArr ! ((ls + p) `mod` 1023)

-- | Recover a GF(32) error magnitude from a GF(1024) discrete log.
--
-- Precondition: @l_e \`mod\` 33 == 0@ (the element lies in the GF(32) subfield).
magnitudeFromLog :: Int -> Word5
magnitudeFromLog l_e =
  -- GF(32) embeds in GF(1024) such that its non-zero elements are exactly
  -- those with GF(1024) discrete log divisible by 33 (= 1023 / 31).
  -- The GF(32) discrete log of the element is (l_e `mod` 1023) `div` 33.
  let normalised = l_e `mod` 1023
      gf32log = normalised `div` 33 -- in [0 .. 30]
  in Word5.fromIntegral (gf32ExpArr ! gf32log :: Int)

--------------------------------------------------------------------------------
-- Syndrome extraction
--
-- Maps the 30-bit polymod residue (6 packed GF(32) elements) to a 30-bit
-- value representing 3 packed GF(1024) elements:
--
--   s0 = bits  0 ..  9
--   s1 = bits 10 .. 19
--   s2 = bits 20 .. 29
--
-- This is a fixed linear map over GF(2), implemented via precomputed XOR
-- constants derived from the Bech32 generator polynomial. The constants are
-- taken directly from sipa's reference implementation.
--------------------------------------------------------------------------------

syndromeFromResidue :: Word32 -> Word32
syndromeFromResidue residue =
  snd $ foldl' step (residue `shiftR` 5, base) syndromeConstants
  where
    low = residue .&. 0x1f
    base = low `xor` (low `shiftL` 10) `xor` (low `shiftL` 20)

    step :: (Word32, Word32) -> Word32 -> (Word32, Word32)
    step (r, acc) k =
      (r `shiftR` 1, if r .&. 1 /= 0 then acc `xor` k else acc)

syndromeConstants :: [Word32]
syndromeConstants =
  [ 0x3d0195bd
  , 0x2a932b53
  , 0x072653af
  , 0x0cdc067e
  , 0x19ac89f5
  , 0x15922369
  , 0x29b443f2
  , 0x03f826ed
  , 0x0574c8fa
  , 0x087935dd
  , 0x2c6dcf4f
  , 0x0acfbfbe
  , 0x158bfa75
  , 0x2993d5e3
  , 0x03b70fc6
  , 0x051e8fd6
  , 0x08b99aa5
  , 0x1167b06a
  , 0x205f60d4
  , 0x12aae581
  , 0x04296874
  , 0x0846f4c1
  , 0x108d4d82
  , 0x210ebf04
  , 0x1299fb28
  ]

--------------------------------------------------------------------------------
-- Encoding internals (unchanged)
--------------------------------------------------------------------------------

separatorChar :: Char
separatorChar = '1'

bech32Const :: Word32
bech32Const = 0x00000001

bech32mConst :: Word32
bech32mConst = 0x2bc830a3

computeChecksum :: Prefix -> Payload -> Checksum
computeChecksum prefix payload =
  Checksum
    (Word5.fromIntegral $ (remainder `shiftR` 25) .&. 0x1f)
    (Word5.fromIntegral $ (remainder `shiftR` 20) .&. 0x1f)
    (Word5.fromIntegral $ (remainder `shiftR` 15) .&. 0x1f)
    (Word5.fromIntegral $ (remainder `shiftR` 10) .&. 0x1f)
    (Word5.fromIntegral $ (remainder `shiftR` 05) .&. 0x1f)
    (Word5.fromIntegral $ (remainder) .&. 0x1f)
  where
    remainder = polymod (values <> replicate 6 0) `xor` bech32Const
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

--------------------------------------------------------------------------------
-- GF(32) antilog table
--
-- GF(32) = GF(2)[x] / (x^5 + x^2 + 1), primitive element α = x = 2.
-- gf32ExpArr ! k = α^k for k in [0 .. 30].
--------------------------------------------------------------------------------

gf32ExpArr :: Array Int Int
gf32ExpArr =
  listArray
    (0, 30)
    [ 1
    , 2
    , 4
    , 8
    , 16
    , 5
    , 10
    , 20
    , 13
    , 26
    , 17
    , 7
    , 14
    , 28
    , 29
    , 31
    , 27
    , 19
    , 3
    , 6
    , 12
    , 24
    , 21
    , 15
    , 30
    , 25
    , 23
    , 11
    , 22
    , 9
    , 18
    ]

--------------------------------------------------------------------------------
-- GF(1024) tables
--
-- GF(1024) = GF(2^10), with discrete log/exp tables of size 1023
-- (multiplicative group order). Taken from sipa's reference implementation.
-- gf1024LogArr ! 0 = -1 (log of zero is undefined; -1 used as sentinel).
--------------------------------------------------------------------------------

gf1024ExpArr :: Array Int Int
gf1024ExpArr =
  listArray
    (0, 1023)
    [ 1
    , 32
    , 311
    , 139
    , 206
    , 553
    , 934
    , 180
    , 537
    , 145
    , 910
    , 131
    , 462
    , 373
    , 652
    , 927
    , 675
    , 840
    , 938
    , 308
    , 235
    , 958
    , 948
    , 756
    , 1007
    , 979
    , 356
    , 172
    , 281
    , 124
    , 240
    , 222
    , 41
    , 23
    , 736
    , 367
    , 460
    , 309
    , 203
    , 649
    , 831
    , 570
    , 454
    , 117
    , 464
    , 693
    , 392
    , 1010
    , 115
    , 272
    , 348
    , 667
    , 383
    , 972
    , 644
    , 671
    , 511
    , 610
    , 129
    , 398
    , 818
    , 922
    , 515
    , 977
    , 292
    , 747
    , 15
    , 480
    , 386
    , 690
    , 360
    , 300
    , 1003
    , 851
    , 202
    , 681
    , 520
    , 689
    , 264
    , 604
    , 630
    , 513
    , 913
    , 867
    , 1021
    , 403
    , 146
    , 1006
    , 1011
    , 83
    , 39
    , 471
    , 597
    , 854
    , 106
    , 560
    , 134
    , 366
    , 492
    , 2
    , 64
    , 583
    , 278
    , 412
    , 370
    , 620
    , 321
    , 315
    , 267
    , 572
    , 262
    , 924
    , 707
    , 56
    , 567
    , 102
    , 944
    , 628
    , 577
    , 470
    , 629
    , 609
    , 225
    , 766
    , 687
    , 712
    , 344
    , 539
    , 209
    , 457
    , 405
    , 82
    , 7
    , 224
    , 734
    , 920
    , 579
    , 406
    , 50
    , 887
    , 381
    , 908
    , 195
    , 905
    , 99
    , 784
    , 749
    , 207
    , 521
    , 657
    , 63
    , 727
    , 696
    , 40
    , 55
    , 983
    , 484
    , 258
    , 796
    , 877
    , 573
    , 294
    , 683
    , 584
    , 246
    , 30
    , 960
    , 772
    , 109
    , 720
    , 600
    , 758
    , 943
    , 404
    , 114
    , 304
    , 107
    , 528
    , 433
    , 485
    , 290
    , 555
    , 998
    , 755
    , 783
    , 269
    , 764
    , 751
    , 143
    , 78
    , 903
    , 419
    , 933
    , 212
    , 361
    , 268
    , 732
    , 984
    , 4
    , 128
    , 430
    , 517
    , 785
    , 717
    , 504
    , 642
    , 607
    , 534
    , 369
    , 524
    , 561
    , 166
    , 89
    , 359
    , 204
    , 617
    , 481
    , 418
    , 901
    , 483
    , 482
    , 450
    , 245
    , 126
    , 176
    , 665
    , 319
    , 395
    , 914
    , 771
    , 141
    , 14
    , 448
    , 181
    , 569
    , 422
    , 773
    , 77
    , 999
    , 723
    , 568
    , 390
    , 562
    , 198
    , 809
    , 250
    , 414
    , 306
    , 43
    , 87
    , 167
    , 121
    , 80
    , 71
    , 679
    , 968
    , 516
    , 817
    , 1018
    , 371
    , 588
    , 118
    , 432
    , 453
    , 21
    , 672
    , 808
    , 218
    , 169
    , 441
    , 229
    , 638
    , 769
    , 205
    , 585
    , 214
    , 297
    , 843
    , 970
    , 580
    , 374
    , 748
    , 239
    , 830
    , 538
    , 241
    , 254
    , 286
    , 156
    , 558
    , 838
    , 618
    , 385
    , 722
    , 536
    , 177
    , 697
    , 8
    , 256
    , 860
    , 298
    , 811
    , 186
    , 985
    , 36
    , 439
    , 293
    , 715
    , 312
    , 363
    , 332
    , 155
    , 718
    , 408
    , 498
    , 962
    , 836
    , 554
    , 966
    , 964
    , 900
    , 451
    , 213
    , 329
    , 59
    , 599
    , 790
    , 557
    , 806
    , 282
    , 28
    , 896
    , 323
    , 379
    , 844
    , 810
    , 154
    , 750
    , 175
    , 377
    , 780
    , 365
    , 396
    , 882
    , 477
    , 789
    , 589
    , 86
    , 135
    , 334
    , 219
    , 137
    , 142
    , 110
    , 688
    , 296
    , 875
    , 765
    , 719
    , 440
    , 197
    , 841
    , 906
    , 3
    , 96
    , 880
    , 413
    , 338
    , 859
    , 458
    , 501
    , 802
    , 410
    , 434
    , 389
    , 594
    , 950
    , 692
    , 424
    , 709
    , 248
    , 478
    , 885
    , 317
    , 459
    , 469
    , 533
    , 273
    , 380
    , 940
    , 500
    , 770
    , 173
    , 313
    , 331
    , 123
    , 16
    , 512
    , 945
    , 596
    , 886
    , 349
    , 699
    , 72
    , 839
    , 586
    , 182
    , 601
    , 726
    , 664
    , 287
    , 188
    , 793
    , 973
    , 676
    , 936
    , 372
    , 684
    , 680
    , 552
    , 902
    , 387
    , 658
    , 95
    , 423
    , 805
    , 378
    , 876
    , 541
    , 17
    , 544
    , 646
    , 735
    , 952
    , 884
    , 285
    , 252
    , 350
    , 731
    , 824
    , 730
    , 792
    , 1005
    , 915
    , 803
    , 442
    , 133
    , 270
    , 668
    , 415
    , 274
    , 284
    , 220
    , 105
    , 592
    , 1014
    , 243
    , 190
    , 857
    , 394
    , 946
    , 564
    , 6
    , 192
    , 1001
    , 787
    , 653
    , 959
    , 916
    , 963
    , 868
    , 797
    , 845
    , 778
    , 429
    , 613
    , 97
    , 848
    , 170
    , 473
    , 917
    , 995
    , 595
    , 918
    , 899
    , 291
    , 523
    , 721
    , 632
    , 961
    , 804
    , 346
    , 603
    , 662
    , 223
    , 9
    , 288
    , 619
    , 417
    , 997
    , 659
    , 127
    , 144
    , 942
    , 436
    , 325
    , 443
    , 165
    , 57
    , 535
    , 337
    , 827
    , 698
    , 104
    , 624
    , 705
    , 120
    , 112
    , 368
    , 556
    , 774
    , 45
    , 151
    , 846
    , 874
    , 733
    , 1016
    , 307
    , 11
    , 352
    , 44
    , 183
    , 633
    , 993
    , 531
    , 465
    , 661
    , 191
    , 889
    , 189
    , 825
    , 762
    , 559
    , 870
    , 861
    , 266
    , 540
    , 49
    , 791
    , 525
    , 529
    , 401
    , 210
    , 425
    , 741
    , 463
    , 341
    , 955
    , 788
    , 621
    , 353
    , 12
    , 384
    , 754
    , 815
    , 58
    , 631
    , 545
    , 678
    , 1000
    , 819
    , 954
    , 820
    , 858
    , 490
    , 194
    , 937
    , 340
    , 923
    , 547
    , 742
    , 431
    , 549
    , 550
    , 582
    , 310
    , 171
    , 505
    , 674
    , 872
    , 669
    , 447
    , 37
    , 407
    , 18
    , 576
    , 502
    , 834
    , 746
    , 47
    , 215
    , 265
    , 636
    , 833
    , 650
    , 863
    , 330
    , 91
    , 295
    , 651
    , 895
    , 125
    , 208
    , 489
    , 162
    , 217
    , 201
    , 713
    , 376
    , 812
    , 90
    , 263
    , 956
    , 1012
    , 179
    , 761
    , 591
    , 22
    , 704
    , 88
    , 327
    , 507
    , 738
    , 303
    , 907
    , 35
    , 343
    , 1019
    , 339
    , 891
    , 253
    , 382
    , 1004
    , 947
    , 532
    , 305
    , 75
    , 807
    , 314
    , 299
    , 779
    , 397
    , 850
    , 234
    , 926
    , 643
    , 639
    , 801
    , 506
    , 706
    , 24
    , 768
    , 237
    , 894
    , 93
    , 487
    , 354
    , 108
    , 752
    , 879
    , 637
    , 865
    , 957
    , 980
    , 388
    , 626
    , 641
    , 575
    , 358
    , 236
    , 862
    , 362
    , 364
    , 428
    , 581
    , 342
    , 987
    , 100
    , 1008
    , 51
    , 855
    , 74
    , 775
    , 13
    , 416
    , 965
    , 932
    , 244
    , 94
    , 391
    , 530
    , 497
    , 930
    , 52
    , 951
    , 660
    , 159
    , 590
    , 54
    , 1015
    , 211
    , 393
    , 978
    , 324
    , 411
    , 402
    , 178
    , 729
    , 888
    , 157
    , 526
    , 625
    , 737
    , 335
    , 251
    , 446
    , 5
    , 160
    , 153
    , 654
    , 991
    , 228
    , 606
    , 566
    , 70
    , 647
    , 767
    , 655
    , 1023
    , 467
    , 725
    , 760
    , 623
    , 289
    , 587
    , 150
    , 878
    , 605
    , 598
    , 822
    , 794
    , 941
    , 468
    , 565
    , 38
    , 503
    , 866
    , 989
    , 164
    , 25
    , 800
    , 474
    , 1013
    , 147
    , 974
    , 708
    , 216
    , 233
    , 1022
    , 499
    , 994
    , 627
    , 673
    , 776
    , 493
    , 34
    , 375
    , 716
    , 472
    , 949
    , 724
    , 728
    , 856
    , 426
    , 645
    , 703
    , 200
    , 745
    , 79
    , 935
    , 148
    , 814
    , 26
    , 832
    , 682
    , 616
    , 449
    , 149
    , 782
    , 301
    , 971
    , 612
    , 65
    , 615
    , 33
    , 279
    , 444
    , 69
    , 743
    , 399
    , 786
    , 685
    , 648
    , 799
    , 781
    , 333
    , 187
    , 1017
    , 275
    , 316
    , 491
    , 226
    , 670
    , 479
    , 853
    , 10
    , 320
    , 283
    , 60
    , 695
    , 456
    , 437
    , 357
    , 140
    , 46
    , 247
    , 62
    , 759
    , 911
    , 163
    , 249
    , 510
    , 578
    , 438
    , 261
    , 1020
    , 435
    , 421
    , 869
    , 829
    , 634
    , 897
    , 355
    , 76
    , 967
    , 996
    , 691
    , 328
    , 27
    , 864
    , 925
    , 739
    , 271
    , 700
    , 168
    , 409
    , 466
    , 757
    , 975
    , 740
    , 495
    , 98
    , 816
    , 986
    , 68
    , 711
    , 184
    , 921
    , 611
    , 161
    , 185
    , 953
    , 852
    , 42
    , 119
    , 400
    , 242
    , 158
    , 622
    , 257
    , 892
    , 29
    , 928
    , 116
    , 496
    , 898
    , 259
    , 828
    , 602
    , 694
    , 488
    , 130
    , 494
    , 66
    , 519
    , 849
    , 138
    , 238
    , 798
    , 813
    , 122
    , 48
    , 823
    , 826
    , 666
    , 351
    , 763
    , 527
    , 593
    , 982
    , 452
    , 53
    , 919
    , 931
    , 20
    , 640
    , 543
    , 81
    , 103
    , 912
    , 835
    , 714
    , 280
    , 92
    , 455
    , 85
    , 231
    , 574
    , 326
    , 475
    , 981
    , 420
    , 837
    , 522
    , 753
    , 847
    , 842
    , 1002
    , 883
    , 509
    , 546
    , 710
    , 152
    , 686
    , 744
    , 111
    , 656
    , 31
    , 992
    , 563
    , 230
    , 542
    , 113
    , 336
    , 795
    , 909
    , 227
    , 702
    , 232
    , 990
    , 196
    , 873
    , 701
    , 136
    , 174
    , 345
    , 571
    , 486
    , 322
    , 347
    , 635
    , 929
    , 84
    , 199
    , 777
    , 461
    , 277
    , 508
    , 514
    , 1009
    , 19
    , 608
    , 193
    , 969
    , 548
    , 518
    , 881
    , 445
    , 101
    , 976
    , 260
    , 988
    , 132
    , 302
    , 939
    , 276
    , 476
    , 821
    , 890
    , 221
    , 73
    , 871
    , 893
    , 61
    , 663
    , 255
    , 318
    , 427
    , 677
    , 904
    , 67
    , 551
    , 614
    ]

gf1024LogArr :: Array Int Int
gf1024LogArr =
  listArray
    (0, 1023)
    [ -1
    , 0
    , 99
    , 363
    , 198
    , 726
    , 462
    , 132
    , 297
    , 495
    , 825
    , 528
    , 561
    , 693
    , 231
    , 66
    , 396
    , 429
    , 594
    , 990
    , 924
    , 264
    , 627
    , 33
    , 660
    , 759
    , 792
    , 858
    , 330
    , 891
    , 165
    , 957
    , 1
    , 804
    , 775
    , 635
    , 304
    , 592
    , 754
    , 90
    , 153
    , 32
    , 883
    , 248
    , 530
    , 521
    , 834
    , 599
    , 911
    , 547
    , 138
    , 689
    , 703
    , 921
    , 708
    , 154
    , 113
    , 508
    , 565
    , 324
    , 828
    , 1013
    , 836
    , 150
    , 100
    , 802
    , 903
    , 1020
    , 874
    , 807
    , 734
    , 253
    , 403
    , 1010
    , 691
    , 646
    , 853
    , 237
    , 189
    , 788
    , 252
    , 927
    , 131
    , 89
    , 982
    , 935
    , 347
    , 249
    , 629
    , 212
    , 620
    , 607
    , 933
    , 664
    , 698
    , 423
    , 364
    , 476
    , 871
    , 144
    , 687
    , 998
    , 115
    , 928
    , 513
    , 453
    , 94
    , 176
    , 667
    , 168
    , 353
    , 955
    , 517
    , 962
    , 174
    , 48
    , 893
    , 43
    , 261
    , 884
    , 516
    , 251
    , 910
    , 395
    , 29
    , 611
    , 223
    , 501
    , 199
    , 58
    , 901
    , 11
    , 1002
    , 446
    , 96
    , 348
    , 973
    , 351
    , 906
    , 3
    , 833
    , 230
    , 352
    , 188
    , 502
    , 9
    , 86
    , 763
    , 790
    , 797
    , 745
    , 522
    , 952
    , 728
    , 336
    , 311
    , 288
    , 719
    , 887
    , 706
    , 727
    , 879
    , 614
    , 839
    , 758
    , 507
    , 211
    , 250
    , 864
    , 268
    , 478
    , 586
    , 27
    , 392
    , 974
    , 338
    , 224
    , 295
    , 716
    , 624
    , 7
    , 233
    , 406
    , 531
    , 876
    , 880
    , 302
    , 816
    , 411
    , 539
    , 457
    , 537
    , 463
    , 992
    , 575
    , 142
    , 970
    , 360
    , 243
    , 983
    , 786
    , 616
    , 74
    , 38
    , 214
    , 273
    , 4
    , 147
    , 612
    , 128
    , 552
    , 710
    , 193
    , 322
    , 275
    , 600
    , 766
    , 615
    , 267
    , 350
    , 452
    , 1009
    , 31
    , 494
    , 133
    , 122
    , 821
    , 966
    , 731
    , 270
    , 960
    , 936
    , 968
    , 767
    , 653
    , 20
    , 679
    , 662
    , 907
    , 282
    , 30
    , 285
    , 886
    , 456
    , 697
    , 222
    , 164
    , 835
    , 380
    , 840
    , 245
    , 724
    , 436
    , 640
    , 286
    , 1015
    , 298
    , 889
    , 157
    , 896
    , 1000
    , 844
    , 110
    , 621
    , 78
    , 601
    , 545
    , 108
    , 195
    , 185
    , 447
    , 862
    , 49
    , 387
    , 450
    , 818
    , 1005
    , 986
    , 102
    , 805
    , 932
    , 28
    , 329
    , 827
    , 451
    , 435
    , 287
    , 410
    , 496
    , 743
    , 180
    , 485
    , 64
    , 306
    , 161
    , 608
    , 355
    , 276
    , 300
    , 649
    , 71
    , 799
    , 1003
    , 633
    , 175
    , 645
    , 247
    , 527
    , 19
    , 37
    , 585
    , 2
    , 308
    , 393
    , 648
    , 107
    , 819
    , 383
    , 1016
    , 226
    , 826
    , 106
    , 978
    , 332
    , 713
    , 505
    , 938
    , 630
    , 857
    , 323
    , 606
    , 394
    , 310
    , 815
    , 349
    , 723
    , 963
    , 510
    , 367
    , 638
    , 577
    , 556
    , 685
    , 636
    , 126
    , 975
    , 491
    , 979
    , 50
    , 401
    , 437
    , 915
    , 529
    , 560
    , 666
    , 852
    , 26
    , 832
    , 678
    , 213
    , 70
    , 194
    , 681
    , 309
    , 682
    , 341
    , 97
    , 35
    , 518
    , 208
    , 104
    , 259
    , 416
    , 13
    , 280
    , 776
    , 618
    , 339
    , 426
    , 333
    , 388
    , 140
    , 641
    , 52
    , 562
    , 292
    , 68
    , 421
    , 674
    , 374
    , 241
    , 699
    , 46
    , 711
    , 459
    , 227
    , 342
    , 651
    , 59
    , 809
    , 885
    , 551
    , 715
    , 85
    , 173
    , 130
    , 137
    , 593
    , 313
    , 865
    , 372
    , 714
    , 103
    , 366
    , 246
    , 449
    , 694
    , 498
    , 217
    , 191
    , 941
    , 847
    , 235
    , 424
    , 378
    , 553
    , 783
    , 1017
    , 683
    , 474
    , 200
    , 581
    , 262
    , 178
    , 373
    , 846
    , 504
    , 831
    , 843
    , 305
    , 359
    , 269
    , 445
    , 506
    , 806
    , 997
    , 725
    , 591
    , 232
    , 796
    , 221
    , 321
    , 920
    , 263
    , 42
    , 934
    , 830
    , 129
    , 369
    , 384
    , 36
    , 985
    , 12
    , 555
    , 44
    , 535
    , 866
    , 739
    , 752
    , 385
    , 119
    , 91
    , 778
    , 479
    , 761
    , 939
    , 1006
    , 344
    , 381
    , 823
    , 67
    , 216
    , 220
    , 219
    , 156
    , 179
    , 977
    , 665
    , 900
    , 613
    , 574
    , 820
    , 98
    , 774
    , 902
    , 870
    , 894
    , 701
    , 314
    , 769
    , 390
    , 370
    , 596
    , 755
    , 204
    , 587
    , 658
    , 631
    , 987
    , 949
    , 841
    , 56
    , 397
    , 81
    , 988
    , 62
    , 256
    , 201
    , 995
    , 904
    , 76
    , 148
    , 943
    , 486
    , 209
    , 549
    , 720
    , 917
    , 177
    , 550
    , 700
    , 534
    , 644
    , 386
    , 207
    , 509
    , 294
    , 8
    , 284
    , 127
    , 546
    , 428
    , 961
    , 926
    , 430
    , 567
    , 950
    , 579
    , 994
    , 582
    , 583
    , 1021
    , 419
    , 5
    , 317
    , 181
    , 519
    , 327
    , 289
    , 542
    , 95
    , 210
    , 242
    , 959
    , 461
    , 753
    , 733
    , 114
    , 240
    , 234
    , 41
    , 976
    , 109
    , 160
    , 937
    , 677
    , 595
    , 118
    , 842
    , 136
    , 279
    , 684
    , 584
    , 101
    , 163
    , 274
    , 405
    , 744
    , 260
    , 346
    , 707
    , 626
    , 454
    , 918
    , 375
    , 482
    , 399
    , 92
    , 748
    , 325
    , 170
    , 407
    , 898
    , 492
    , 79
    , 747
    , 732
    , 206
    , 991
    , 121
    , 57
    , 878
    , 801
    , 475
    , 1022
    , 803
    , 795
    , 215
    , 291
    , 497
    , 105
    , 559
    , 888
    , 742
    , 514
    , 721
    , 675
    , 771
    , 117
    , 120
    , 80
    , 566
    , 488
    , 532
    , 850
    , 980
    , 602
    , 670
    , 271
    , 656
    , 925
    , 676
    , 205
    , 655
    , 54
    , 784
    , 431
    , 735
    , 812
    , 39
    , 604
    , 609
    , 14
    , 466
    , 729
    , 737
    , 956
    , 149
    , 422
    , 500
    , 705
    , 536
    , 493
    , 1014
    , 409
    , 225
    , 914
    , 51
    , 448
    , 590
    , 822
    , 55
    , 265
    , 772
    , 588
    , 16
    , 414
    , 1018
    , 568
    , 254
    , 418
    , 75
    , 794
    , 162
    , 417
    , 811
    , 953
    , 124
    , 354
    , 77
    , 69
    , 856
    , 377
    , 45
    , 899
    , 829
    , 152
    , 296
    , 512
    , 402
    , 863
    , 972
    , 967
    , 785
    , 628
    , 515
    , 659
    , 112
    , 765
    , 379
    , 951
    , 875
    , 125
    , 617
    , 931
    , 307
    , 777
    , 203
    , 312
    , 358
    , 169
    , 487
    , 293
    , 239
    , 780
    , 740
    , 408
    , 151
    , 781
    , 717
    , 440
    , 438
    , 196
    , 525
    , 134
    , 432
    , 34
    , 722
    , 632
    , 861
    , 869
    , 554
    , 580
    , 808
    , 954
    , 787
    , 598
    , 65
    , 281
    , 146
    , 337
    , 187
    , 668
    , 944
    , 563
    , 183
    , 23
    , 867
    , 171
    , 837
    , 741
    , 625
    , 541
    , 916
    , 186
    , 357
    , 123
    , 736
    , 661
    , 272
    , 391
    , 229
    , 167
    , 236
    , 520
    , 692
    , 773
    , 984
    , 473
    , 650
    , 340
    , 814
    , 798
    , 184
    , 145
    , 202
    , 810
    , 465
    , 558
    , 345
    , 326
    , 548
    , 441
    , 412
    , 750
    , 964
    , 158
    , 471
    , 908
    , 813
    , 760
    , 657
    , 371
    , 444
    , 490
    , 425
    , 328
    , 647
    , 266
    , 244
    , 335
    , 301
    , 619
    , 909
    , 791
    , 564
    , 872
    , 257
    , 60
    , 570
    , 572
    , 1007
    , 749
    , 912
    , 439
    , 540
    , 913
    , 511
    , 897
    , 849
    , 283
    , 40
    , 793
    , 603
    , 597
    , 930
    , 316
    , 942
    , 290
    , 404
    , 17
    , 361
    , 946
    , 277
    , 334
    , 472
    , 523
    , 945
    , 477
    , 905
    , 652
    , 73
    , 882
    , 824
    , 93
    , 690
    , 782
    , 458
    , 573
    , 368
    , 299
    , 544
    , 680
    , 605
    , 859
    , 671
    , 756
    , 83
    , 470
    , 848
    , 543
    , 1011
    , 589
    , 971
    , 524
    , 356
    , 427
    , 159
    , 746
    , 669
    , 365
    , 996
    , 343
    , 948
    , 434
    , 382
    , 400
    , 139
    , 718
    , 538
    , 1008
    , 639
    , 890
    , 1012
    , 663
    , 610
    , 331
    , 851
    , 895
    , 484
    , 320
    , 218
    , 420
    , 190
    , 1019
    , 143
    , 362
    , 634
    , 141
    , 965
    , 10
    , 838
    , 929
    , 82
    , 228
    , 443
    , 468
    , 480
    , 483
    , 922
    , 135
    , 877
    , 61
    , 578
    , 111
    , 860
    , 654
    , 15
    , 892
    , 981
    , 702
    , 923
    , 696
    , 192
    , 6
    , 789
    , 415
    , 576
    , 18
    , 1004
    , 389
    , 751
    , 503
    , 172
    , 116
    , 398
    , 460
    , 643
    , 22
    , 779
    , 376
    , 704
    , 433
    , 881
    , 571
    , 557
    , 622
    , 672
    , 21
    , 467
    , 166
    , 489
    , 315
    , 469
    , 319
    , 695
    , 318
    , 854
    , 255
    , 993
    , 278
    , 800
    , 53
    , 413
    , 764
    , 868
    , 999
    , 63
    , 712
    , 25
    , 673
    , 940
    , 919
    , 155
    , 197
    , 303
    , 873
    , 686
    , 1001
    , 757
    , 969
    , 730
    , 958
    , 533
    , 770
    , 481
    , 855
    , 499
    , 182
    , 238
    , 569
    , 464
    , 947
    , 72
    , 642
    , 442
    , 87
    , 24
    , 688
    , 989
    , 47
    , 88
    , 623
    , 762
    , 455
    , 709
    , 526
    , 817
    , 258
    , 637
    , 845
    , 84
    , 768
    , 738
    ]
