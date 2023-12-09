-- https://adventofcode.com/2022/day/2
module AOC2022.Day02 (solve) where
import           AOC.Prelude
import           AOC (aoc)
import           AOC.Parser (Parser, sepEndBy1, eol, space)

parser :: Parser [(Int, Int)]
parser = ((,) <$> select1 <* space <*> select2) `sepEndBy1` eol where
    select1 = 0 <$ "A" <|> 1 <$ "B" <|> 2 <$ "C"
    select2 = 0 <$ "X" <|> 1 <$ "Y" <|> 2 <$ "Z"

score :: (Int, Int) -> Int
score (s1, s2) = 3 * ((s2 - s1 + 1) `mod` 3) + s2 + 1

score' :: (Int, Int) -> Int
score' (s1, s2) = score (s1, (s1 + s2 + 2) `mod` 3)

part1 :: [(Int, Int)] -> Int
part1 = sum . map score

part2 :: [(Int, Int)] -> Int
part2 = sum . map score'

solve :: Text -> IO ()
solve = aoc parser part1 part2