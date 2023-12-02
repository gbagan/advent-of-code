-- https://adventofcode.com/2023/day/2
module AOC2023.Day02 (solve) where
import           RIO
import           Text.Megaparsec (sepEndBy1)
import           Text.Megaparsec.Char (char, eol, string)
import           Text.Megaparsec.Char.Lexer (decimal)
import           Util (Parser, aoc)

data Color = Red | Green | Blue

data Game = Game Int [(Int, Color)]

parser :: Parser [Game]
parser = game `sepEndBy1` eol where
    game = do
        _ <- string "Game "
        id_ <- decimal
        _ <- string ": "
        sets_ <- set_ `sepEndBy1` (string "; " <|> string ", ")
        return (Game id_ sets_)
    set_ = do
        n <- decimal
        _ <- char ' '
        color <- Blue <$ string "blue" <|> Red <$ string "red" <|> Green <$ string "green"
        return (n, color)

solve1 :: Int -> Int -> Int -> [Game] -> Int
solve1 red green blue = sum . map solveGame where
    solveGame (Game id_ sets_) | all isPossibleSet sets_ = id_
                               | otherwise = 0
    isPossibleSet (n, color) = case color of
        Red -> n <= red
        Green -> n <= green
        Blue -> n <= blue

solve2 :: [Game] -> Int
solve2 = sum . map solveGame where
    solveGame (Game _ sets_) = let (r, g, b) = foldl' go (0, 0, 0) sets_ in r * g * b
    go (red, green, blue) (n, color) = case color of
        Red -> (max red n, green, blue)
        Green -> (red, max n green, blue)
        Blue -> (red, green, max n blue)

solve :: MonadIO m => Text -> m ()
solve = aoc parser (solve1 12 13 14) solve2