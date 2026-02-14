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
  , fromText
  , toText
  )
where

import Control.Monad ((<=<), (>=>))
import Data.Bech32.HumanReadableChar (HumanReadableChar)
import Data.Bech32.HumanReadableChar qualified as HumanReadableChar
import Data.Data (Proxy (Proxy))
import Data.Foldable qualified as Foldable
import Data.Function ((&))
import Data.Kind (Constraint)
import Data.List.NonEmpty qualified as List (NonEmpty)
import Data.List.NonEmpty qualified as List.NonEmpty
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Type.Bool (If)
import Data.Type.Ord (OrderingI (EQI, GTI, LTI))
import Data.Vector.Sized (Vector)
import Data.Vector.Sized qualified as Vector
import GHC.TypeError (Assert)
import GHC.TypeLits
  ( ConsSymbol
  , KnownNat
  , KnownSymbol
  , Nat
  , SomeNat (SomeNat)
  , Symbol
  , cmpNat
  , someNatVal
  , type (-)
  , type (<=)
  )
import Numeric.Natural (Natural)

data HumanReadablePart
  = forall length.
    (MinLength <= length, length <= MaxLength) =>
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

fromList :: List.NonEmpty HumanReadableChar -> Maybe HumanReadablePart
fromList (List.NonEmpty.toList -> list) = do
  SomeNat (listLength :: Proxy listLength) <- naturalLength list
  LEQ <- minLength `assertLEQ` listLength
  LEQ <- listLength `assertLEQ` maxLength
  HumanReadablePart <$> Vector.fromList @listLength list
  where
    naturalLength :: Foldable f => f a -> Maybe SomeNat
    naturalLength = someNatVal . fromIntegral @Int @Integer . Foldable.length

type family KnownValidSymbol (s :: Symbol) :: Constraint where
  KnownValidSymbol s =
    (KnownSymbol s)

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

{-
type family ValidSymbol (s :: Symbol) :: Either ParseError () where
  ValidSymbol s =
    If (SymbolNonEmpty s) SymbolEmpty ()
-}
type family SymbolEmpty (s :: Symbol) :: Bool where
  SymbolEmpty "" = True
  SymbolEmpty __ = False

type family RepeatChar (n :: Nat) (c :: Char) :: Symbol where
  RepeatChar 0 c = ""
  RepeatChar n c = ConsSymbol c (RepeatChar (n - 1) c)

-- >>> fromSymbol @""
-- ...
-- ... A Bech32 prefix must have at least one character.
-- ...
--
-- >>> fromSymbol @(RepeatChar 84 'A')
-- ...
-- ... A Bech32 prefix may not be longer than 83 characters.
-- ...
--
-- >>> fromSymbol @"AAAA±AAAA"
-- ...
--     • "AAAA±AAAA"
--            ^
--       Invalid character.
--       A Bech32 prefix may only contain characters from the range ['!'..'~'].
-- ...
--
fromSymbol :: forall s. KnownValidSymbol s => HumanReadablePart
fromSymbol = undefined

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

maybeToEither :: a -> Maybe b -> Either a b
maybeToEither a (Just b) = Right b
maybeToEither a Nothing = Left a
