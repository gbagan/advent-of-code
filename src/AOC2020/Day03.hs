-- https://adventofcode.com/2020/day/3
module AOC2020.Day03 (solve) where
import           RIO hiding (some)
import           Text.Megaparsec (sepEndBy1, some)
import           Text.Megaparsec.Char (char, eol)
import           Util (Parser, aoc, count)
import           Util.Matrix (Matrix)
import qualified Util.Matrix as M

parser :: Parser (Matrix Bool)
parser = M.fromList <$> some c `sepEndBy1` eol where
    c = False <$ char '.' <|> True <$ char '#'

countTrees :: Int -> Int -> Matrix Bool -> Int
countTrees drow dcol mat = [0..m-1] & count \i -> mat M.! (i*drow, i * dcol `mod` n) where
    n = M.nbColumns mat
    m = M.nbRows mat `div` drow

part1 :: Matrix Bool -> Int
part1 = countTrees 1 3

part2 :: Matrix Bool -> Int
part2 mat = product [ countTrees drow dcol mat
                    | (drow, dcol) <- [(1, 1), (1, 3), (1, 5), (1, 7), (2, 1)]
                    ]

solve :: MonadIO m => Text -> m ()
solve = aoc parser part1 part2
