module AOC.Parser 
    ( module P
    , module C
    , module L
    , Parser
    , BinParser
    , signedDecimal
    , bitP
    , skipLine) where

import           AOC.Prelude
import           Text.Megaparsec as P (Parsec, anySingle, anySingleBut, between, choice, count, many, manyTill, 
                                        sepBy, sepBy1, sepEndBy1, optional, some, takeP, takeRest, takeWhileP, try)
import           Text.Megaparsec.Char as C (alphaNumChar, char, digitChar, eol, letterChar, space, hspace, hexDigitChar, lowerChar, numberChar, upperChar)
import           Text.Megaparsec.Char.Lexer as L (decimal, lexeme)

type Parser = Parsec Void Text
type BinParser = P.Parsec Void [Bool]

signedDecimal :: Num a => Parser a
signedDecimal = L.decimal <|> C.char '-' *> (negate <$> L.decimal)

bitP :: Parser Bool
bitP = False <$ C.char '0' <|> True <$ C.char '1'

skipLine :: Parser ()
skipLine = void $ P.takeWhileP Nothing (/= '\n') *> C.eol