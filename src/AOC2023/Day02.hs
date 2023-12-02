-- https://adventofcode.com/2023/day/2
module AOC2023.Day02 (solve) where
import           RIO hiding (id, sets)
import           Text.Megaparsec (sepEndBy1)
import           Text.Megaparsec.Char (char, eol, string)
import           Text.Megaparsec.Char.Lexer (decimal)
import           Util (Parser, aoc)

data Game = Game Int [(Int, Int, Int)]

parser :: Parser [Game]
parser = game `sepEndBy1` eol where
    game = do
        id <- string "Game " *> decimal <* string ": "
        Game id <$> colorSet `sepEndBy1` (string "; " <|> string ", ")
    colorSet = do
        n <- decimal <* char ' '
        (n, 0, 0) <$ string "red" <|> (0, n, 0) <$ string "green" <|> (0, 0, n) <$ string "blue"

solve1 :: [Game] -> Int
solve1 = sum . map solveGame where
    solveGame (Game id sets) | all isPossibleSet sets = id
                             | otherwise = 0
    isPossibleSet (r, g, b) = r <= 12 && g <= 13 && b <= 14

solve2 :: [Game] -> Int
solve2 = sum . map solveGame where
    solveGame (Game _ sets) = power . foldl' maxSet (0, 0, 0) $ sets
    maxSet (r, g, b) (r', g', b') = (max r r', max g g', max b b')
    power (r, g, b) = r * g * b

solve :: MonadIO m => Text -> m ()
solve = aoc parser solve1 solve2