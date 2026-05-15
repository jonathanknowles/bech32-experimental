{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Codec.Bech32.Prefix
  ( -- * Type
    Prefix

    -- * Construction
  , fromList
  , fromSymbol
  , fromText
  , FromTextError (..)

    -- * Conversion
  , toList
  , toText

    -- * Attributes
  , length
  )
where

import Codec.Bech32.Prefix.Char (PrefixChar)
import Codec.Bech32.Prefix.Char qualified as PrefixChar
import Codec.Bech32.Prefix.Char.Types qualified as PrefixChar
import Codec.Bech32.Utilities
  ( AssertSymbolNotEmpty
  , InvalidCharError
  , fromRight
  , maybeToEither
  )
import Control.Monad ((>=>))
import Data.Data (Proxy (Proxy))
import Data.Foldable qualified as Foldable
import Data.Foldable1 (Foldable1 (toNonEmpty))
import Data.Kind (Constraint)
import Data.List.NonEmpty qualified as List (NonEmpty)
import Data.List.NonEmpty qualified as List.NonEmpty
import Data.Sequence.NonEmpty (NESeq)
import Data.Sequence.NonEmpty qualified as NESeq
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Type.Bool (If)
import GHC.TypeLits
  ( KnownSymbol
  , Nat
  , Symbol
  , UnconsSymbol
  , symbolVal
  , type (+)
  )
import Numeric.Natural (Natural)
import Text.Read (Lexeme (Ident, Punc), Read (readPrec), lexP, parens, prec)
import Prelude hiding (length)

-- $setup
-- >>> :set -XDataKinds
-- >>> :set -XOverloadedLists
-- >>> :set -XOverloadedStrings
-- >>> :set -XTypeApplications
-- >>> import Prelude hiding (length)

--------------------------------------------------------------------------------
-- Type
--------------------------------------------------------------------------------

-- | A valid Bech32 prefix.
newtype Prefix = Prefix (NESeq PrefixChar)
  deriving newtype (Eq, Ord, Semigroup)

instance Read Prefix where
  readPrec = parens $ prec 10 $ do
    Ident "fromSymbol" <- lexP
    Punc "@" <- lexP
    unsafeFromText <$> readPrec

instance Show Prefix where
  showsPrec d hrp =
    showParen (d > 10) $
      showString "fromSymbol @" . shows (toText hrp)

--------------------------------------------------------------------------------
-- Construction from lists
--------------------------------------------------------------------------------

-- | Constructs a 'Prefix' from a list of characters.
--
-- Assuming the following import:
--
-- >>> import Codec.Bech32.Prefix.Char (fromChar)
--
-- We can then write:
--
-- >>> fromList [fromChar @'A', fromChar @'B', fromChar @'C', fromChar @'D']
-- fromSymbol @"ABCD"
fromList :: List.NonEmpty PrefixChar -> Prefix
fromList = Prefix . NESeq.fromList

--------------------------------------------------------------------------------
-- Construction from symbols
--------------------------------------------------------------------------------

-- | Constructs a 'Prefix' from a type-level textual 'Symbol'.
--
-- >>> fromSymbol @"ABCD"
-- fromSymbol @"ABCD"
--
-- Symbols must be non-empty:
--
-- >>> fromSymbol @""
-- ...
-- ... Expected a non-empty symbol.
-- ...
--
-- Symbols must not contain invalid characters:
--
-- >>> fromSymbol @"ABCD EFGH"
-- ...
--     • "ABCD EFGH"
--            ^
--       Invalid character at indicated position.
--       Expected a character in the range ['!' .. '~'].
-- ...
fromSymbol :: forall s. KnownValidSymbol s => Prefix
fromSymbol =
  fromRight handleFailure $ fromText $ Text.pack $ symbolVal $ Proxy @s
  where
    handleFailure e =
      error $ "Prefix.fromSymbol: unexpected failure:" <> show e

type family KnownValidSymbol (s :: Symbol) :: Constraint where
  KnownValidSymbol s =
    ( KnownSymbol s
    , AssertSymbolNotEmpty s
    , AssertSymbolCharsValid s
    )

type family AssertSymbolCharsValid (s :: Symbol) :: Constraint where
  AssertSymbolCharsValid s = AssertSymbolCharsValidInner (SymbolCharInvalid s)

type family
  AssertSymbolCharsValidInner
    (n :: Maybe (Symbol, Nat))
    :: Constraint
  where
  AssertSymbolCharsValidInner Nothing = ()
  AssertSymbolCharsValidInner (Just '(s, n)) =
    InvalidCharError s n PrefixChar.InvalidChar

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
      (PrefixChar.ValidChar c)
      (SymbolCharInvalidInner s0 (UnconsSymbol s) (n + 1))
      (Just '(s0, n))

--------------------------------------------------------------------------------
-- Construction from text
--------------------------------------------------------------------------------

-- | Constructs a 'Prefix' from text.
--
-- >>> fromText "ABCD"
-- Right (fromSymbol @"ABCD")
--
-- The text must not be non-empty:
--
-- >>> fromText ""
-- Left FromTextErrorEmpty
--
-- The text must not contain invalid characters:
--
-- >>> fromText "ABCD EFGH"
-- Left (FromTextErrorInvalidChar 4)
fromText :: Text -> Either FromTextError Prefix
fromText = assertCharsValid >=> assertNotEmpty
  where
    assertCharsValid :: Text -> Either FromTextError [PrefixChar]
    assertCharsValid = traverse parseChar . zip [0 ..] . Text.unpack
      where
        parseChar (n, c) =
          maybeToEither
            (FromTextErrorInvalidChar n)
            (PrefixChar.fromCharMaybe c)

    assertNotEmpty :: [PrefixChar] -> Either FromTextError Prefix
    assertNotEmpty =
      maybeToEither FromTextErrorEmpty . fmap fromList . List.NonEmpty.nonEmpty

unsafeFromText :: Text -> Prefix
unsafeFromText = fromRight onFailure . fromText
  where
    onFailure = error "unsafeFromText"

data FromTextError
  = -- | Indicates that the given 'Text' is empty.
    FromTextErrorEmpty
  | -- | Indicates that the character at the given 0-based index is not valid.
    FromTextErrorInvalidChar Natural
  deriving (Eq, Show)

--------------------------------------------------------------------------------
-- Conversion to lists
--------------------------------------------------------------------------------

-- | Converts a 'Prefix' to a list of characters.
--
-- >>> toList (fromSymbol @"ABCD")
-- fromChar @'A' :| [fromChar @'B',fromChar @'C',fromChar @'D']
toList :: Prefix -> List.NonEmpty PrefixChar
toList (Prefix cs) = toNonEmpty cs

--------------------------------------------------------------------------------
-- Conversion to text
--------------------------------------------------------------------------------

-- | Converts a 'Prefix' to text.
--
-- >>> toText (fromSymbol @"ABCD")
-- "ABCD"
toText :: Prefix -> Text
toText (Prefix cs) =
  Text.pack $ PrefixChar.toChar <$> Foldable.toList cs

--------------------------------------------------------------------------------
-- Attributes
--------------------------------------------------------------------------------

-- | Computes the length of a 'Prefix'.
--
-- >>> length (fromSymbol @"ABCD")
-- 4
length :: Prefix -> Int
length (Prefix cs) = NESeq.length cs
