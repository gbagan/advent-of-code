-- https://adventofcode.com/2020/day/18
module Day18 (solve) where
import           AOC.Prelude
import           AOC (aoc)
import           Text.Megaparsec (parseMaybe)
import           AOC.Parser (Parser, takeRest, between, sepEndBy1, eol, decimal, hspace, lexeme)
import           Control.Monad.Combinators.Expr (Operator(..), makeExprParser)

lex :: Parser a -> Parser a
lex = lexeme hspace

add, mul :: Parser (Integer -> Integer -> Integer)
add = (+) <$ lex "+"
mul = (*) <$ lex "*"

table1, table2 :: [[Operator Parser Integer]]
table1 = [[ InfixL add, InfixL mul]]
table2 = [[InfixL add], [InfixL mul]]

parseWith :: [[Operator Parser Integer]] -> Text -> Maybe Integer
parseWith table = parseMaybe exprs where
    exprs = sum <$> expr `sepEndBy1` eol
    expr = makeExprParser term table
    term = parens expr <|> lex decimal
    parens = between (lex "(") (lex ")")

solve :: Text -> IO ()
solve = aoc takeRest (parseWith table1) (parseWith table2)