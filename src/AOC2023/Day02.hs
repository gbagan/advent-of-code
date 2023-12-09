-- https://adventofcode.com/2023/day/2
module AOC2023.Day02 (solve) where
import           Relude hiding (id)
import           Util (aoc)
import           Util.Parser (Parser, decimal, eol, sepEndBy1)

data RGB = RGB Int Int Int
data Game = Game Int [RGB]

parser :: Parser [Game]
parser = game `sepEndBy1` eol where
    game = do
        id <- "Game " *> decimal <* ": "
        Game id <$> colorSet `sepEndBy1` ("; " <|> ", ")
    colorSet = do
        n <- decimal <* " "
        RGB n 0 0 <$ "red" <|> RGB 0 n 0 <$ "green" <|> RGB 0 0 n <$ "blue"

part1 :: [Game] -> Int
part1 = sum . map solveGame where
    solveGame (Game id sets) | all isPossibleSet sets = id
                             | otherwise = 0
    isPossibleSet (RGB r g b) = r <= 12 && g <= 13 && b <= 14

part2 :: [Game] -> Int
part2 = sum . map solveGame where
    solveGame (Game _ sets) = power . foldl' maxSet (RGB 0 0 0) $ sets
    maxSet (RGB r g b) (RGB r' g' b') = RGB (max r r') (max g g') (max b b')
    power (RGB r g b) = r * g * b

solve :: MonadIO m => Text -> m ()
solve = aoc parser part1 part2