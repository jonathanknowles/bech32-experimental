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

import Data.Kind (Constraint)
import Data.Type.Bool (Not, type (&&))
import Data.Type.Equality (type (==))
import GHC.TypeError (Assert, TypeError)
import GHC.TypeError qualified as TypeError
import GHC.TypeLits
  ( CmpChar
  , CmpNat
  , KnownChar
  , KnownNat
  , Nat
  )

type family KnownValidChar (c :: Char) :: Constraint where
  KnownValidChar c =
    ( KnownChar c
    , Assert (ValidChar c) (TypeError (TypeError.Text InvalidChar))
    )

type family KnownValidOrdinal (c :: Nat) :: Constraint where
  KnownValidOrdinal c =
    ( KnownNat c
    , Assert (ValidOrdinal c) (TypeError (TypeError.Text InvalidOrdinal))
    )

type family ValidChar (c :: Char) :: Bool where
  ValidChar c =
    (&&)
      (Not (CmpChar c '!' == 'LT))
      (Not (CmpChar c '~' == 'GT))

type family ValidOrdinal (c :: Nat) :: Bool where
  ValidOrdinal c =
    (&&)
      (Not (CmpNat c 033 == 'LT))
      (Not (CmpNat c 126 == 'GT))

type InvalidChar =
  "Expected a character in the range ['!' .. '~']."

type InvalidOrdinal =
  "Expected an ordinal in the range [33 .. 126]."
