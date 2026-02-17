{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Bech32.DataPart where

import Data.Bech32.DataChar qualified as DataChar
import Data.Foldable qualified as Foldable
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Word5 (Word5)
import Text.Read (Lexeme (Ident, Symbol), Read (readPrec), lexP, parens)
import Prelude hiding (words)

-- Note that the checksum part is not stored; this ensures that values of this
-- type are correct by construction.
--
-- But actually:
--
-- data part = payload + checksum
--
newtype DataPart = DataPart (Seq Word5)
  deriving stock (Eq, Ord)
  deriving newtype (Monoid, Semigroup)

instance Read DataPart where
  readPrec = parens $ do
    Ident "DataPart" <- lexP
    Symbol "." <- lexP
    Ident "fromList" <- lexP
    fromList <$> readPrec

-- fromSymbol
-- This would definitely be the most concise representation for Show.
-- More concise than fromList.

instance Show DataPart where
  showsPrec _ dp =
    showString "DataPart.fromList " . shows (toList dp)

fromList :: [Word5] -> DataPart
fromList words = DataPart (Seq.fromList words)

toList :: DataPart -> [Word5]
toList (DataPart words) = Foldable.toList words

fromText :: Text -> Maybe DataPart
fromText t =
  DataPart . Seq.fromList <$> traverse charToWord5 (Text.unpack t)
  where
    charToWord5 :: Char -> Maybe Word5
    charToWord5 = fmap DataChar.toWord5 <$> DataChar.fromCharMaybe

fromTextWithChecksum :: Text -> Either () DataPart
fromTextWithChecksum = undefined

toText :: DataPart -> Text
toText (DataPart words) =
  Text.pack $ word5ToChar <$> Foldable.toList words
  where
    word5ToChar :: Word5 -> Char
    word5ToChar = DataChar.toChar . DataChar.fromWord5

toTextWithChecksum :: DataPart -> Text
toTextWithChecksum = undefined
