{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ViewPatterns #-}

module Data.Bech32.HumanReadableChar
  ( HumanReadableChar
  , fromChar
  , toChar
  )
where

import Data.Char (chr)
import Prelude hiding (maxBound, minBound)

newtype HumanReadableChar = HumanReadableChar Char
  deriving newtype (Eq, Ord)

minBound :: HumanReadableChar
minBound = HumanReadableChar $ chr 33

maxBound :: HumanReadableChar
maxBound = HumanReadableChar $ chr 126

fromChar :: Char -> Maybe HumanReadableChar
fromChar (HumanReadableChar -> c)
  | c < minBound = Nothing
  | c > maxBound = Nothing
  | otherwise = Just c

toChar :: HumanReadableChar -> Char
toChar (HumanReadableChar c) = c
