-- https://adventofcode.com/2021/day/2
module AOC2021.Day02 (solve) where
import           AOC.Prelude
import           AOC (aoc)
import           AOC.Parser (Parser, decimal, eol, sepEndBy1)

data Instr = Forward !Int | IDown !Int

parser :: Parser [Instr]
parser = line `sepEndBy1` eol where
    line = Forward <$> ("forward " *> decimal)
       <|> IDown <$> ("down " *> decimal)
       <|> IDown . negate <$> ("up " *> decimal)

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

solve :: Text -> IO ()
solve = aoc parser part1 part2
