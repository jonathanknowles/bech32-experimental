{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

module Data.Bech32.HumanReadablePart
  ( HumanReadablePart
  , fromList
  ) where

import Data.Bech32.HumanReadableChar (HumanReadableChar)
import Data.Data (Proxy (Proxy))
import Data.List.NonEmpty qualified as List (NonEmpty)
import Data.List.NonEmpty qualified as List.NonEmpty
import Data.Type.Ord (OrderingI (EQI, GTI, LTI))
import Data.Vector.Sized (Vector)
import Data.Vector.Sized qualified as Vector
import GHC.TypeLits (KnownNat, SomeNat (SomeNat), cmpNat, someNatVal)
import GHC.TypeNats (Nat, type (<=))

data HumanReadablePart
  = forall n.
    (1 <= n, n <= 83) =>
    HumanReadablePart (Vector n HumanReadableChar)

deriving instance Show HumanReadablePart

fromList :: List.NonEmpty HumanReadableChar -> Maybe HumanReadablePart
fromList (List.NonEmpty.toList -> cs) = do
  SomeNat (n :: Proxy n) <- someNatVal (fromIntegral @Int @Integer $ length cs)
  LE <- minLength `assertLE` n
  LE <- n `assertLE` maxLength
  HumanReadablePart <$> Vector.fromList @n cs

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

data LE (a :: Nat) (b :: Nat) where
  LE :: a <= b => LE a b

assertLE
  :: forall a b
   . KnownNat a
  => KnownNat b
  => Proxy a
  -> Proxy b
  -> Maybe (LE a b)
assertLE proxyA proxyB = case proxyA `cmpNat` proxyB of
  LTI -> Just LE
  EQI -> Just LE
  GTI -> Nothing

minLength :: Proxy 1
minLength = Proxy @1

maxLength :: Proxy 83
maxLength = Proxy @83
