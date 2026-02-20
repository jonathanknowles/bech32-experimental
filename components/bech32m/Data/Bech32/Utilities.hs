{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Bech32.Utilities where

import Data.Kind (Constraint)
import GHC.TypeError (ErrorMessage (type (:$$:)), TypeError)
import GHC.TypeError qualified as TypeError
import GHC.TypeLits (AppendSymbol, ConsSymbol, Nat, Symbol, type (+), type (-))

fromRight :: (a -> b) -> Either a b -> b
fromRight = (`either` id)

maybeToEither :: a -> Maybe b -> Either a b
maybeToEither _ (Just b) = Right b
maybeToEither a Nothing = Left a

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

type family SymbolEmpty (s :: Symbol) :: Bool where
  SymbolEmpty "" = True
  SymbolEmpty __ = False
