-- https://adventofcode.com/2020/day/3
module AOC2020.Day03 (solve) where
import           Relude hiding (some)
import           Data.Massiv.Array (Matrix, (!), fromLists', size, U, Comp(Seq), Ix2(..), Sz(..))
import           AOC (aoc)
import           AOC.List (count)
import           AOC.Parser (Parser, sepEndBy1, some, eol)

parser :: Parser (Matrix U Bool)
parser = fromLists' Seq <$> some c `sepEndBy1` eol where
    c = False <$ "." <|> True <$ "#"

countTrees :: Int -> Int -> Matrix U Bool -> Int
countTrees drow dcol mat = [0..(m `div` drow) - 1] & count \i -> mat ! Ix2 (i*drow) (i * dcol `mod` n) where
    Sz2 m n = size mat

part1 :: Matrix U Bool -> Int
part1 = countTrees 1 3

part2 :: Matrix U Bool -> Int
part2 mat = product [ countTrees drow dcol mat
                    | (drow, dcol) <- [(1, 1), (1, 3), (1, 5), (1, 7), (2, 1)]
                    ]

solve :: Text -> IO ()
solve = aoc parser part1 part2
