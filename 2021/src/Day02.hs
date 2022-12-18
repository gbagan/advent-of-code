-- https://adventofcode.com/2021/day/2
module Day02 (solve) where
import           RIO
import           Text.Megaparsec (sepEndBy1)
import           Text.Megaparsec.Char (string, eol)
import           Text.Megaparsec.Char.Lexer (decimal)
import           Util (Parser, aoc)

data Instr = Forward !Int | IDown !Int

parser :: Parser [Instr]
parser = line `sepEndBy1` eol where
    line = Forward <$> (string "forward " *> decimal)
       <|> IDown <$> (string "down " *> decimal)
       <|> IDown . negate <$> (string "up " *> decimal)

part1 :: [Instr] -> Int
part1 l = tx * ty where
        (tx, ty) =
            foldl' (\(x, y) -> \case
                Forward v -> (x + v, y)
                IDown v   -> (x, y + v)
            ) (0, 0) l

part2 :: [Instr] -> Int
part2 l = tx * ty where
        (tx, ty, _) =
            foldl' (\(x, y, aim) -> \case
                Forward v -> (x + v, y + aim * v, aim)
                IDown v   -> (x, y, aim + v)
            ) (0, 0, 0) l

solve :: (HasLogFunc env) => Text -> RIO env ()
solve = aoc parser part1 part2
