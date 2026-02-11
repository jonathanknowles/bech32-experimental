{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE StandaloneDeriving #-}

module Data.Bech32.HumanReadablePart where

import Data.Bech32.HumanReadableChar (HumanReadableChar)
import Data.Bech32.HumanReadableChar qualified as HumanReadableChar
import GHC.TypeNats (Nat, type (+), type (<=))

data HumanReadablePart
  = forall n.
    (1 <= n, n <= 83) =>
    HumanReadablePart (BuildHumanReadablePart n)

deriving instance Show HumanReadablePart

-- fromSymbol

{-
instance Show HumanReadablePart where
  showsPrec _ dp =
    showString "HumanReadablePart " . shows (toList dp)
-}

infixr 5 :+

data BuildHumanReadablePart (n :: Nat) where
  Singleton :: HumanReadableChar -> BuildHumanReadablePart 1
  (:+)
    :: HumanReadableChar
    -> BuildHumanReadablePart n
    -> BuildHumanReadablePart (n + 1)

deriving instance Show (BuildHumanReadablePart n)

example :: BuildHumanReadablePart 2
example = HumanReadableChar.minBound :+ Singleton HumanReadableChar.maxBound
