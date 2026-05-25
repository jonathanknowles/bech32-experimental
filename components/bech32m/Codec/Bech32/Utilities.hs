{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Codec.Bech32.Utilities where

import Data.Kind (Constraint, Type)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Type.Bool (Not, type (&&))
import Data.Type.Equality (type (==))
import GHC.TypeError (Assert, ErrorMessage (type (:$$:)), TypeError)
import GHC.TypeError qualified as TypeError
import GHC.TypeLits
  ( AppendSymbol
  , CmpChar
  , CmpNat
  , ConsSymbol
  , Nat
  , Symbol
  , type (+)
  , type (-)
  )

fromRight :: (a -> b) -> Either a b -> b
fromRight = (`either` id)

maybeToEither :: a -> Maybe b -> Either a b
maybeToEither _ (Just b) = Right b
maybeToEither a Nothing = Left a

splitOnLast :: Char -> Text -> Maybe (Text, Text)
splitOnLast c t =
  case Text.breakOnEnd (Text.singleton c) t of
    ("", _) ->
      Nothing
    (prefixWith1, suffix) ->
      Just (Text.init prefixWith1, suffix)

type family
  InvalidCharError
    (invalidSymbol :: Symbol)
    (charIndex :: Nat)
    (message)
    :: Constraint
  where
  InvalidCharError invalidSymbol charIndex message =
    TypeError
      ( TypeError.ShowType
          invalidSymbol
          :$$: TypeError.Text (InvalidCharErrorArrow (charIndex + 1))
          :$$: TypeError.Text "Invalid character at indicated position."
          :$$: message
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

type family SymbolEmpty (s :: Symbol) :: Bool where
  SymbolEmpty "" = True
  SymbolEmpty __ = False

type family AssertSymbolNotEmpty (s :: Symbol) :: Constraint where
  AssertSymbolNotEmpty s =
    Assert
      (Not (SymbolEmpty s))
      (TypeError (TypeError.Text SymbolEmptyErrorMessage))

type SymbolEmptyErrorMessage =
  "Expected a non-empty symbol."

data Interval (k :: Type) = k :..: k

type family
  CharWithinInclusiveInterval
    (n :: Char)
    (range :: Interval Char)
    :: Bool
  where
  CharWithinInclusiveInterval (n :: Char) (lo :..: hi) =
    (&&)
      (Not (CmpChar n lo == 'LT))
      (Not (CmpChar n hi == 'GT))

type family
  NatWithinInclusiveInterval
    (n :: Nat)
    (range :: Interval Nat)
    :: Bool
  where
  NatWithinInclusiveInterval (n :: Nat) (lo :..: hi) =
    (&&)
      (Not (CmpNat n lo == 'LT))
      (Not (CmpNat n hi == 'GT))
