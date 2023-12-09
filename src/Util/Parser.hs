module Util.Parser 
    ( module P
    , module C
    , module L
    , Parser
    , BinParser
    , signedDecimal
    , bitP
    , skipLine) where

import           Relude
import           Text.Megaparsec as P (Parsec, anySingleBut, choice, many, sepBy1, sepEndBy1, some, takeWhileP)
import           Text.Megaparsec.Char as C (alphaNumChar, char, eol, space, hspace, upperChar)
import           Text.Megaparsec.Char.Lexer as L (decimal)
import           Util (Parser)

type BinParser = P.Parsec Void [Bool]

signedDecimal :: (Num a) => Parser a
signedDecimal = L.decimal <|> C.char '-' *> (negate <$> L.decimal)

bitP :: Parser Bool
bitP = False <$ C.char '0' <|> True <$ C.char '1'

skipLine :: Parser ()
skipLine = void $ P.takeWhileP Nothing (/= '\n') *> C.eol