{-# LANGUAGE DerivingStrategies #-}

module Data.BitOrder where

-- FromLeastToMostSignificant
-- FromMostToLeastSignificant
data BitOrder
  = FromLSBToMSB
  | FromMSBToLSB
  deriving stock (Bounded, Enum, Eq, Ord, Show)
