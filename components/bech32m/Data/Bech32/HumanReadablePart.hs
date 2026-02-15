{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

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
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Type.Bool (If, Not)
import Data.Type.Ord (OrderingI (EQI, GTI, LTI), type (<?))
import Data.Vector.Sized (Vector)
import Data.Vector.Sized qualified as Vector
import GHC.TypeError (Assert, ErrorMessage (type (:$$:)), TypeError)
import GHC.TypeError qualified as TypeError
import GHC.TypeLits
  ( AppendSymbol
  , ConsSymbol
  , KnownNat
  , KnownSymbol
  , Nat
  , SomeNat (SomeNat)
  , Symbol
  , UnconsSymbol
  , cmpNat
  , someNatVal
  , symbolVal
  , type (+)
  , type (-)
  , type (<=)
  )
import Numeric.Natural (Natural)
import Prelude hiding (length)
import GHC.TypeNats (natVal)

-- the max length of 83 characters comes from
-- 90 - 6 (min length of data part) - 1 (length of separator).
--
-- Is there any point to limiting it here? Really, the actual limit depends on
-- the length of the oerall Bech32 string, which we don't have access to.
--
-- By getting rid of the limit here, we can allow this to be a normal semigroup
-- with append.
--
data HumanReadablePart
  = forall length.
    (KnownNat length, MinLength <= length, length <= MaxLength) =>
    HumanReadablePart (Vector length HumanReadableChar)

instance Eq HumanReadablePart where
  HumanReadablePart a == HumanReadablePart b =
    Vector.fromSized a == Vector.fromSized b

instance Ord HumanReadablePart where
  compare (HumanReadablePart a) (HumanReadablePart b) =
    compare (Vector.fromSized a) (Vector.fromSized b)

instance Show HumanReadablePart where
  showsPrec d hrp =
    showParen (d > 10) $
      showString "fromSymbol @" . shows (toText hrp)

length :: HumanReadablePart -> Natural
length (HumanReadablePart (_ :: Vector length HumanReadableChar)) =
  natVal (Proxy @length)

fromList :: List.NonEmpty HumanReadableChar -> Maybe HumanReadablePart
fromList (List.NonEmpty.toList -> list) = do
  SomeNat (listLength :: Proxy listLength) <- naturalLength list
  LEQ <- minLength `assertLEQ` listLength
  LEQ <- listLength `assertLEQ` maxLength
  HumanReadablePart <$> Vector.fromList @listLength list
  where
    naturalLength :: Foldable f => f a -> Maybe SomeNat
    naturalLength = someNatVal . fromIntegral @Int @Integer . Foldable.length

-- >>> fromSymbol @""
-- ...
-- ... A Bech32 prefix must have at least one character.
-- ...
--
-- >>> fromSymbol @(ReplicateChar 84 'A')
-- ...
-- ... A Bech32 prefix may not be longer than 83 characters.
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
    , AssertSymbolNotTooLong s
    , AssertSymbolCharsValid s
    )

type family AssertSymbolNotEmpty (s :: Symbol) :: Constraint where
  AssertSymbolNotEmpty s =
    Assert
      (Not (SymbolEmpty s))
      (TypeError (TypeError.Text SymbolEmptyErrorMessage))

type SymbolEmptyErrorMessage =
  "A Bech32 prefix must have at least one character."

type family AssertSymbolNotTooLong (s :: Symbol) :: Constraint where
  AssertSymbolNotTooLong s =
    Assert
      (Not (SymbolTooLong s))
      (TypeError (TypeError.Text SymbolTooLongErrorMessage))

type SymbolTooLongErrorMessage =
  "A Bech32 prefix may not be longer than 83 characters."

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
  | ParseErrorTooLong
  | ParseErrorInvalidChar Natural
  deriving (Eq, Show)

fromText :: Text -> Either ParseError HumanReadablePart
fromText =
  (assertNotTooLong <=< assertNotEmpty <=< assertHumanReadable) . Text.unpack
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
      -> Either ParseError (List.NonEmpty HumanReadableChar)
    assertNotEmpty = maybeToEither ParseErrorEmpty . List.NonEmpty.nonEmpty

    assertNotTooLong
      :: List.NonEmpty HumanReadableChar
      -> Either ParseError HumanReadablePart
    assertNotTooLong = maybeToEither ParseErrorTooLong . fromList

type family SymbolEmpty (s :: Symbol) :: Bool where
  SymbolEmpty "" = True
  SymbolEmpty __ = False

type family SymbolTooLong (s :: Symbol) :: Bool where
  SymbolTooLong s = 83 <? SymbolLength s

type family SymbolLength (s :: Symbol) :: Nat where
  SymbolLength s = SymbolLengthInner (UnconsSymbol s) 0

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
  Text.pack $ HumanReadableChar.toChar <$> Vector.toList cs

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

data LEQ (a :: Nat) (b :: Nat) where
  LEQ :: a <= b => LEQ a b

assertLEQ
  :: forall a b
   . KnownNat a
  => KnownNat b
  => Proxy a
  -> Proxy b
  -> Maybe (LEQ a b)
assertLEQ proxyA proxyB = case proxyA `cmpNat` proxyB of
  LTI -> Just LEQ
  EQI -> Just LEQ
  GTI -> Nothing

fromRight :: (a -> b) -> Either a b -> b
fromRight = (`either` id)

type MinLength = 1

type MaxLength = 83

minLength :: Proxy MinLength
minLength = Proxy @MinLength

maxLength :: Proxy MaxLength
maxLength = Proxy @MaxLength

maybeToEither :: a -> Maybe b -> Either a b
maybeToEither _ (Just b) = Right b
maybeToEither a Nothing = Left a
