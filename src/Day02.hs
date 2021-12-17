-- https://adventofcode.com/2021/day/2
module Day02 (solve) where

import           Data.List (foldl')
import           Text.Megaparsec (sepEndBy1, (<|>))
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as L
import           Util (Parser, aocTemplate)

data Instr = Forward Int | Down Int

parser :: Parser [Instr]
parser = line `sepEndBy1` P.eol where
    line = Forward <$> (P.string "forward " *> L.decimal)
       <|> Down <$> (P.string "down " *> L.decimal)
       <|> Down . negate <$> (P.string "up " *> L.decimal)

part1 :: [Instr] -> Int
part1 l = tx * ty where
        (tx, ty) =
            foldl' (\(x, y) -> \case
                Forward v -> (x + v, y)
                Down v    -> (x, y + v)
            ) (0, 0) l

part2 :: [Instr] -> Int
part2 l = tx * ty where
        (tx, ty, _) =
            foldl' (\(x, y, aim) -> \case
                Forward v -> (x + v, y + aim * v, aim)
                Down v    -> (x, y, aim + v)
            ) (0, 0, 0) l

solve :: String -> IO ()
solve = aocTemplate parser (pure . part1) (pure . part2)
