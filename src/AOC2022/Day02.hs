-- https://adventofcode.com/2022/day/2
module AOC2022.Day02 (solve) where
import           RIO
import           Text.Megaparsec (sepEndBy1)
import           Text.Megaparsec.Char (char, space, eol)
import           Util (Parser, aoc)

parser :: Parser [(Int, Int)]
parser = ((,) <$> select1 <* space <*> select2) `sepEndBy1` eol where
    select1 = 0 <$ char 'A' <|> 1 <$ char 'B' <|> 2 <$ char 'C'
    select2 = 0 <$ char 'X' <|> 1 <$ char 'Y' <|> 2 <$ char 'Z'

score :: (Int, Int) -> Int
score (s1, s2) = 3 * ((s2 - s1 + 1) `mod` 3) + s2 + 1

score' :: (Int, Int) -> Int
score' (s1, s2) = score (s1, (s1 + s2 + 2) `mod` 3)

part1 :: [(Int, Int)] -> Int
part1 = sum . map score

part2 :: [(Int, Int)] -> Int
part2 = sum . map score'

solve :: MonadIO m => Text -> m ()
solve = aoc parser part1 part2