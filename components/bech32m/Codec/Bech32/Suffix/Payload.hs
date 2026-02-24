{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

{- HLINT ignore "Use newtype instead of data" -}

module Codec.Bech32.Suffix.Payload
  ( Payload
  , fromWord5List
  , fromWord8List
  , toWord5List
  , toWord8List
  , fromSymbol
  , fromText
  , toText
  , length
  )
where

import Codec.Bech32.Suffix.Char qualified as SuffixChar
import Codec.Bech32.Utilities (InvalidCharError, fromRight, maybeToEither)
import Data.Bifunctor (Bifunctor (first))
import Data.Bit (Bit (B0, B1))
import Data.BitSeq qualified as BitSeq
import Data.Bits (FiniteBits)
import Data.Foldable qualified as Foldable
import Data.Kind (Constraint)
import Data.Proxy (Proxy (Proxy))
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Type.Bool (If)
import Data.Word (Word8)
import Data.Word5 (Word5)
import GHC.TypeLits
  ( KnownSymbol
  , Symbol
  , UnconsSymbol
  , symbolVal
  , type (+)
  )
import GHC.TypeNats (Nat)
import Numeric.Natural (Natural)
import Text.Read (Lexeme (Ident, Punc), Read (readPrec), lexP, parens, prec)
import Prelude hiding (length, words)

-- $setup
-- >>> :set -XDataKinds
-- >>> :set -XTypeApplications

newtype Payload = Payload (Seq Word5)
  deriving stock (Eq, Ord)
  deriving newtype (Monoid, Semigroup)

instance Read Payload where
  readPrec = parens $ prec 10 $ do
    Ident "fromSymbol" <- lexP
    Punc "@" <- lexP
    unsafeFromText <$> readPrec

instance Show Payload where
  showsPrec d p =
    showParen (d > 10) $
      showString "fromSymbol @" . shows (toText p)

length :: Payload -> Int
length (Payload cs) = Seq.length cs

-- | Constructs a 'Payload' from a list of words.
--
-- >>> fromWord5List [0 .. 31]
-- fromSymbol @"QPZRY9X8GF2TVDW0S3JN54KHCE6MUA7L"
fromWord5List :: [Word5] -> Payload
fromWord5List words = Payload (Seq.fromList words)

toWord5List :: Payload -> [Word5]
toWord5List (Payload words) = Foldable.toList words

fromWord8List :: [Word8] -> Payload
fromWord8List = fromWord5List . resliceInflate B0

toWord8List :: Payload -> Maybe [Word8]
toWord8List ws
  | B1 `elem` remainder = Nothing
  | otherwise = Just result
  where
    (remainder, result) = resliceDeflate (toWord5List ws)

-- | Constructs a 'Payload' from a type-level textual symbol.
--
-- >>> fromSymbol @""
-- fromSymbol @""
--
-- >>> fromSymbol @"PQRS"
-- fromSymbol @"PQRS"
--
-- >>> fromSymbol @"ABCD"
-- ...
--     • "ABCD"
--         ^
--       Invalid character at indicated position.
--       Expected a character from the set [023456789ACDEFGHJKLMNPQRSTUVWXYZ].
-- ...
fromSymbol :: forall s. KnownValidSymbol s => Payload
fromSymbol =
  fromRight handleFailure $ fromText $ Text.pack $ symbolVal $ Proxy @s
  where
    handleFailure e =
      error $ "Payload.fromSymbol: unexpected failure:" <> show e

type family KnownValidSymbol (s :: Symbol) :: Constraint where
  KnownValidSymbol s =
    ( KnownSymbol s
    , AssertSymbolCharsValid s
    )

type family AssertSymbolCharsValid (s :: Symbol) :: Constraint where
  AssertSymbolCharsValid s = AssertSymbolCharsValidInner (SymbolCharInvalid s)

type family SymbolCharInvalid (s :: Symbol) :: Maybe (Symbol, Nat) where
  SymbolCharInvalid s = SymbolCharInvalidInner s (UnconsSymbol s) 0

type family
  SymbolCharInvalidInner
    (s :: Symbol)
    (m :: Maybe (Char, Symbol))
    (n :: Nat)
    :: Maybe (Symbol, Nat)
  where
  SymbolCharInvalidInner _ Nothing _ = Nothing
  SymbolCharInvalidInner s0 (Just '(c, s)) n =
    If
      (SuffixChar.ValidChar c)
      (SymbolCharInvalidInner s0 (UnconsSymbol s) (n + 1))
      (Just '(s0, n))

type family
  AssertSymbolCharsValidInner
    (n :: Maybe (Symbol, Nat))
    :: Constraint
  where
  AssertSymbolCharsValidInner Nothing = ()
  AssertSymbolCharsValidInner (Just '(s, n)) =
    InvalidCharError s n InvalidCharErrorMessage

type InvalidCharErrorMessage =
  "Expected a character from the set [023456789ACDEFGHJKLMNPQRSTUVWXYZ]."

data ParseError
  = ParseErrorInvalidChar Natural
  deriving (Eq, Show)

fromText :: Text -> Either ParseError Payload
fromText = fmap fromWord5List . traverse parseChar . zip [0 ..] . Text.unpack
  where
    parseChar (n, c) =
      maybeToEither
        (ParseErrorInvalidChar n)
        (SuffixChar.toWord5 <$> SuffixChar.fromCharMaybe c)

unsafeFromText :: Text -> Payload
unsafeFromText = fromRight onFailure . fromText
  where
    onFailure = error "unsafeFromText"

toText :: Payload -> Text
toText (Payload words) =
  Text.pack $ word5ToChar <$> Foldable.toList words
  where
    word5ToChar :: Word5 -> Char
    word5ToChar = SuffixChar.toChar . SuffixChar.fromWord5

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

resliceInflate :: (FiniteBits a, FiniteBits b) => Bit -> [a] -> [b]
resliceInflate padding =
  BitSeq.toChunksInflate padding . BitSeq.fromChunks

resliceDeflate :: (FiniteBits a, FiniteBits b) => [a] -> ([Bit], [b])
resliceDeflate =
  first BitSeq.toList . BitSeq.toChunksDeflate . BitSeq.fromChunks
