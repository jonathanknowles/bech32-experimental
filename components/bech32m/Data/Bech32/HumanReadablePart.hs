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
import GHC.TypeError (Assert, TypeError)
import GHC.TypeError qualified as TypeError
import GHC.TypeLits
  ( ConsSymbol
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
      (TypeError (TypeError.Text "EMPTY"))

type family AssertSymbolNotTooLong (s :: Symbol) :: Constraint where
  AssertSymbolNotTooLong s =
    Assert
      (Not (SymbolTooLong s))
      (TypeError (TypeError.Text "TOO LONG"))

type family AssertSymbolCharsValid (s :: Symbol) :: Constraint where
  AssertSymbolCharsValid s = AssertSymbolCharsValidInner (SymbolCharInvalid s)

type family AssertSymbolCharsValidInner (n :: Maybe Nat) :: Constraint where
  AssertSymbolCharsValidInner Nothing = ()
  AssertSymbolCharsValidInner (Just n) = TypeError (TypeError.Text "CHAR")

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

type family SymbolCharInvalid (s :: Symbol) :: Maybe Nat where
  SymbolCharInvalid s = SymbolCharInvalidInner (UnconsSymbol s) 0

type family
  SymbolCharInvalidInner
    (m :: Maybe (Char, Symbol))
    (n :: Nat)
    :: Maybe Nat
  where
  SymbolCharInvalidInner Nothing _ = Nothing
  SymbolCharInvalidInner (Just '(c, s)) n =
    If
      (HumanReadableChar.ValidChar c)
      (SymbolCharInvalidInner (UnconsSymbol s) (n + 1))
      (Just n)

type family
  SymbolLengthInner
    (m :: Maybe (Char, Symbol))
    (n :: Nat)
    :: Nat
  where
  SymbolLengthInner Nothing n = n
  SymbolLengthInner (Just '(c, s)) n =
    SymbolLengthInner (UnconsSymbol s) (n + 1)

type family RepeatChar (n :: Nat) (c :: Char) :: Symbol where
  RepeatChar 0 c = ""
  RepeatChar n c = ConsSymbol c (RepeatChar (n - 1) c)

exampleSymbol :: HumanReadablePart
exampleSymbol = fromSymbol @"ABCD"

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
