module Util.Parser where

import           RIO
import           Text.Megaparsec (Parsec, takeWhileP)
import           Text.Megaparsec.Char (eol, char)
import           Text.Megaparsec.Char.Lexer (decimal)
import           Util (Parser)

type BinParser = Parsec Void [Bool]

signedDecimal :: (Num a) => Parser a
signedDecimal = decimal <|> char '-' *> (negate <$> decimal)

bitP :: Parser Bool
bitP = False <$ char '0' <|> True <$ char '1'

skipLine :: Parser ()
skipLine = void $ takeWhileP Nothing (/= '\n') *> eol