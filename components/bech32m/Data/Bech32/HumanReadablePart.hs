{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

module Data.Bech32.HumanReadablePart
  ( HumanReadablePart
  , fromList
  )
where

import Data.Bech32.HumanReadableChar (HumanReadableChar)
import Data.Bech32.HumanReadableChar qualified as HumanReadableChar
import Data.Data (Proxy (Proxy))
import Data.List.NonEmpty qualified as List (NonEmpty)
import Data.List.NonEmpty qualified as List.NonEmpty
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Type.Ord (OrderingI (EQI, GTI, LTI))
import Data.Vector.Sized (Vector)
import Data.Vector.Sized qualified as Vector
import GHC.TypeLits (KnownNat, SomeNat (SomeNat), cmpNat, someNatVal)
import GHC.TypeNats (Nat, type (<=))

data HumanReadablePart
  = forall n.
    (MinLength <= n, n <= MaxLength) =>
    HumanReadablePart (Vector n HumanReadableChar)

instance Eq HumanReadablePart where
  HumanReadablePart a == HumanReadablePart b =
    Vector.fromSized a == Vector.fromSized b

instance Show HumanReadablePart where
  showsPrec _ hrp =
    showString "fromSymbol @" . shows (toText hrp)

fromList :: List.NonEmpty HumanReadableChar -> Maybe HumanReadablePart
fromList (List.NonEmpty.toList -> cs) = do
  SomeNat (n :: Proxy n) <- someNatVal (fromIntegral @Int @Integer $ length cs)
  LEQ <- minLength `assertLEQ` n
  LEQ <- n `assertLEQ` maxLength
  HumanReadablePart <$> Vector.fromList @n cs

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

type MinLength = 1
type MaxLength = 83

minLength :: Proxy MinLength
minLength = Proxy @MinLength

maxLength :: Proxy MaxLength
maxLength = Proxy @MaxLength
