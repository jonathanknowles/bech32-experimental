{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Codec.Bech32.Prefix.Char.Types
  ( KnownValidChar
  , KnownValidOrdinal
  , ValidChar
  , ValidOrdinal
  , InvalidChar
  , InvalidOrdinal
  )
where

import Codec.Bech32.Utilities
  ( CharWithinInclusiveInterval
  , Interval (..)
  , NatWithinInclusiveInterval
  )
import Data.Kind (Constraint)
import Data.Type.Bool (type (||))
import GHC.TypeError (Assert, ErrorMessage (type (:$$:)), TypeError)
import GHC.TypeError qualified as TypeError
import GHC.TypeLits
  ( KnownChar
  , KnownNat
  , Nat
  )

type family KnownValidChar (c :: Char) :: Constraint where
  KnownValidChar c =
    ( KnownChar c
    , Assert (ValidChar c) (TypeError InvalidChar)
    )

type family KnownValidOrdinal (c :: Nat) :: Constraint where
  KnownValidOrdinal c =
    ( KnownNat c
    , Assert (ValidOrdinal c) (TypeError InvalidOrdinal)
    )

type family ValidChar (c :: Char) :: Bool where
  ValidChar c =
    (||)
      (CharWithinInclusiveInterval c ('!' :..: '@'))
      (CharWithinInclusiveInterval c ('[' :..: '~'))

type family ValidOrdinal (c :: Nat) :: Bool where
  ValidOrdinal c =
    (||)
      (NatWithinInclusiveInterval c (033 :..: 064))
      (NatWithinInclusiveInterval c (091 :..: 126))

type InvalidChar =
  TypeError.Text
    "Expected a character in one of the following inclusive intervals:"
    :$$: TypeError.Text "  ['!' .. '@']"
    :$$: TypeError.Text "  ['[' .. '~']"

type InvalidOrdinal =
  TypeError.Text
    "Expected an ordinal in one of the following inclusive intervals:"
    :$$: TypeError.Text "  [33 ..  64]"
    :$$: TypeError.Text "  [91 .. 126]"
