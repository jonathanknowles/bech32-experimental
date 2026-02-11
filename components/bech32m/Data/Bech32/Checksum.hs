{-# LANGUAGE DerivingStrategies #-}

module Data.Bech32.Checksum where

import Data.Word5 (Word5)

data Checksum = Checksum
  { c0 :: !Word5
  , c1 :: !Word5
  , c2 :: !Word5
  , c3 :: !Word5
  , c4 :: !Word5
  , c5 :: !Word5
  }
  deriving stock (Eq, Ord, Read, Show)
