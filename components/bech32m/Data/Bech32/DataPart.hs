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

module Data.Bech32.DataPart
  ( DataPart
  , fromList
  , toList
  , fromSymbol
  , length
  )
where

import Data.Bech32.DataChar qualified as DataChar
import Data.Foldable qualified as Foldable
import Data.Kind (Constraint)
import Data.Proxy (Proxy (Proxy))
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Type.Bool (If)
import Data.Word5 (Word5)
import GHC.TypeError (ErrorMessage (type (:$$:)), TypeError)
import GHC.TypeError qualified as TypeError
import GHC.TypeLits
  ( AppendSymbol
  , ConsSymbol
  , KnownSymbol
  , Symbol
  , UnconsSymbol
  , symbolVal
  , type (+)
  , type (-)
  )
import GHC.TypeNats (Nat)
import Numeric.Natural (Natural)
import Text.Read (Lexeme (Ident, Punc), Read (readPrec), lexP, parens, prec)
import Prelude hiding (length, words)

-- |
-- $setup
-- >>> :set -XDataKinds
-- >>> :set -XTypeApplications

-- Note that the checksum part is not stored; this ensures that values of this
-- type are correct by construction.
--
-- But actually:
--
-- data part = payload + checksum
--
newtype DataPart = DataPart (Seq Word5)
  deriving stock (Eq, Ord)
  deriving newtype (Monoid, Semigroup)

instance Read DataPart where
  readPrec = parens $ prec 10 $ do
    Ident "fromSymbol" <- lexP
    Punc "@" <- lexP
    unsafeFromText <$> readPrec

instance Show DataPart where
  showsPrec d hrp =
    showParen (d > 10) $
      showString "fromSymbol @" . shows (toText hrp)

length :: DataPart -> Int
length (DataPart cs) = Seq.length cs

fromList :: [Word5] -> DataPart
fromList words = DataPart (Seq.fromList words)

toList :: DataPart -> [Word5]
toList (DataPart words) = Foldable.toList words

-- | Constructs a 'DataPart' from a type-level textual symbol.
--
-- >>> fromSymbol @"AAAA"
-- fromSymbol @"AAAA"
--
-- >>> fromSymbol @"AAAA AAAA"
-- ...
--     • "AAAA AAAA"
--            ^
--       Invalid character at indicated position.
--       Characters allowed: [023456789ACDEFGHJKLMNPQRSTUVWXYZ].
-- ...
--
fromSymbol :: forall s. KnownValidSymbol s => DataPart
fromSymbol =
  fromRight handleFailure $ fromText $ Text.pack $ symbolVal $ Proxy @s
  where
    handleFailure e =
      error $ "DataPart.fromSymbol: unexpected failure:" <> show e

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
      (DataChar.ValidChar c)
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
  "Characters allowed: [023456789ACDEFGHJKLMNPQRSTUVWXYZ]."

type family
  InvalidCharError
    (invalidSymbol :: Symbol)
    (charIndex :: Nat)
    (message :: Symbol)
    :: Constraint
  where
  InvalidCharError invalidSymbol charIndex message =
    TypeError
      ( TypeError.ShowType
          invalidSymbol
          :$$: TypeError.Text (InvalidCharErrorArrow (charIndex + 1))
          :$$: TypeError.Text "Invalid character at indicated position."
          :$$: TypeError.Text message
      )

type InvalidCharErrorArrow n = ReplicateChar n ' ' `AppendSymbol` "^"

type family ReplicateChar (n :: Nat) (c :: Char) :: Symbol where
  ReplicateChar n c = ReplicateCharInner "" n c

type family
  ReplicateCharInner
    (s :: Symbol)
    (n :: Nat)
    (c :: Char)
    :: Symbol
  where
  ReplicateCharInner s 0 _ = s
  ReplicateCharInner s n c = ReplicateCharInner (ConsSymbol c s) (n - 1) c

data ParseError
  = ParseErrorInvalidChar Natural
  deriving (Eq, Show)

fromText :: Text -> Either ParseError DataPart
fromText = fmap fromList . traverse parseChar . zip [0 ..] . Text.unpack
  where
    parseChar (n, c) =
      maybeToEither
        (ParseErrorInvalidChar n)
        (DataChar.toWord5 <$> DataChar.fromCharMaybe c)

unsafeFromText :: Text -> DataPart
unsafeFromText = fromRight onFailure . fromText
  where
    onFailure = error "unsafeFromText"

toText :: DataPart -> Text
toText (DataPart words) =
  Text.pack $ word5ToChar <$> Foldable.toList words
  where
    word5ToChar :: Word5 -> Char
    word5ToChar = DataChar.toChar . DataChar.fromWord5

maybeToEither :: a -> Maybe b -> Either a b
maybeToEither _ (Just b) = Right b
maybeToEither a Nothing = Left a

fromRight :: (a -> b) -> Either a b -> b
fromRight = (`either` id)
