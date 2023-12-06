-- https://adventofcode.com/2023/day/2
module AOC2023.Day02 (solve) where
import           RIO hiding (id, sets)
import           Text.Megaparsec (sepEndBy1)
import           Text.Megaparsec.Char (eol)
import           Text.Megaparsec.Char.Lexer (decimal)
import           Util (Parser, aoc)

data Game = Game Int [(Int, Int, Int)]

parser :: Parser [Game]
parser = game `sepEndBy1` eol where
    game = do
        id <- "Game " *> decimal <* ": "
        Game id <$> colorSet `sepEndBy1` ("; " <|> ", ")
    colorSet = do
        n <- decimal <* " "
        (n, 0, 0) <$ "red" <|> (0, n, 0) <$ "green" <|> (0, 0, n) <$ "blue"

part1 :: [Game] -> Int
part1 = sum . map solveGame where
    solveGame (Game id sets) | all isPossibleSet sets = id
                             | otherwise = 0
    isPossibleSet (r, g, b) = r <= 12 && g <= 13 && b <= 14

part2 :: [Game] -> Int
part2 = sum . map solveGame where
    solveGame (Game _ sets) = power . foldl' maxSet (0, 0, 0) $ sets
    maxSet (r, g, b) (r', g', b') = (max r r', max g g', max b b')
    power (r, g, b) = r * g * b

solve :: MonadIO m => Text -> m ()
solve = aoc parser part1 part2