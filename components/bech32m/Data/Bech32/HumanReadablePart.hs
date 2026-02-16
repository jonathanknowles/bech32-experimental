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

module Data.Bech32.HumanReadablePart
  ( HumanReadablePart
  , fromList
  , fromSymbol
  , length
  )
where

import Control.Monad ((<=<))
import Data.Bech32.HumanReadableChar (HumanReadableChar)
import Data.Bech32.HumanReadableChar qualified as HumanReadableChar
import Data.Data (Proxy (Proxy))
import Data.Foldable qualified as Foldable
import Data.Kind (Constraint)
import Data.List.NonEmpty qualified as List (NonEmpty)
import Data.List.NonEmpty qualified as List.NonEmpty
import Data.Sequence.NonEmpty (NESeq)
import Data.Sequence.NonEmpty qualified as NESeq
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Type.Bool (If, Not)
import GHC.TypeError (Assert, ErrorMessage (type (:$$:)), TypeError)
import GHC.TypeError qualified as TypeError
import GHC.TypeLits
  ( AppendSymbol
  , ConsSymbol
  , KnownSymbol
  , Nat
  , Symbol
  , UnconsSymbol
  , symbolVal
  , type (+)
  , type (-)
  )
import Numeric.Natural (Natural)
import Prelude hiding (length)

newtype HumanReadablePart = HumanReadablePart (NESeq HumanReadableChar)
  deriving newtype (Eq, Ord, Semigroup)

instance Show HumanReadablePart where
  showsPrec d hrp =
    showParen (d > 10) $
      showString "fromSymbol @" . shows (toText hrp)

length :: HumanReadablePart -> Int
length (HumanReadablePart cs) = NESeq.length cs

fromList :: List.NonEmpty HumanReadableChar -> HumanReadablePart
fromList = HumanReadablePart . NESeq.fromList

-- >>> fromSymbol @""
-- ...
-- ... A Bech32 prefix must have at least one character.
-- ...
--
-- >>> fromSymbol @"AAAA AAAA"
-- ...
--     • "AAAA AAAA"
--            ^
--       Invalid character at indicated position.
--       A Bech32 prefix may only contain characters from the range ['!'..'~'].
-- ...
--
fromSymbol :: forall s. KnownValidSymbol s => HumanReadablePart
fromSymbol =
  fromRight handleFailure $ fromText $ Text.pack $ symbolVal $ Proxy @s
  where
    handleFailure e =
      error $ "HumanReadablePart.fromSymbol: unexpected failure:" <> show e

type family KnownValidSymbol (s :: Symbol) :: Constraint where
  KnownValidSymbol s =
    ( KnownSymbol s
    , AssertSymbolNotEmpty s
    , AssertSymbolCharsValid s
    )

type family AssertSymbolNotEmpty (s :: Symbol) :: Constraint where
  AssertSymbolNotEmpty s =
    Assert
      (Not (SymbolEmpty s))
      (TypeError (TypeError.Text SymbolEmptyErrorMessage))

type SymbolEmptyErrorMessage =
  "A Bech32 prefix must have at least one character."

type family AssertSymbolCharsValid (s :: Symbol) :: Constraint where
  AssertSymbolCharsValid s = AssertSymbolCharsValidInner (SymbolCharInvalid s)

type family
  AssertSymbolCharsValidInner
    (n :: Maybe (Symbol, Nat))
    :: Constraint
  where
  AssertSymbolCharsValidInner Nothing = ()
  AssertSymbolCharsValidInner (Just '(s, n)) =
    InvalidCharError s n InvalidCharErrorMessage

type InvalidCharErrorMessage =
  "A Bech32 prefix may only contain characters from the range ['!'..'~']."

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

data ParseError
  = ParseErrorEmpty
  | ParseErrorInvalidChar Natural
  deriving (Eq, Show)

fromText :: Text -> Either ParseError HumanReadablePart
fromText =
  (assertNotEmpty <=< assertHumanReadable) . Text.unpack
  where
    assertHumanReadable :: [Char] -> Either ParseError [HumanReadableChar]
    assertHumanReadable = traverse parseChar . zip [0 ..]
      where
        parseChar (n, c) =
          maybeToEither
            (ParseErrorInvalidChar n)
            (HumanReadableChar.fromCharMaybe c)

    assertNotEmpty
      :: [HumanReadableChar]
      -> Either ParseError HumanReadablePart
    assertNotEmpty =
      maybeToEither ParseErrorEmpty . fmap fromList . List.NonEmpty.nonEmpty

type family SymbolEmpty (s :: Symbol) :: Bool where
  SymbolEmpty "" = True
  SymbolEmpty __ = False

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
      (HumanReadableChar.ValidChar c)
      (SymbolCharInvalidInner s0 (UnconsSymbol s) (n + 1))
      (Just '(s0, n))

type family
  SymbolLengthInner
    (m :: Maybe (Char, Symbol))
    (n :: Nat)
    :: Nat
  where
  SymbolLengthInner Nothing n = n
  SymbolLengthInner (Just '(c, s)) n =
    SymbolLengthInner (UnconsSymbol s) (n + 1)

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

toText :: HumanReadablePart -> Text
toText (HumanReadablePart cs) =
  Text.pack $ HumanReadableChar.toChar <$> Foldable.toList cs

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

fromRight :: (a -> b) -> Either a b -> b
fromRight = (`either` id)

maybeToEither :: a -> Maybe b -> Either a b
maybeToEither _ (Just b) = Right b
maybeToEither a Nothing = Left a
