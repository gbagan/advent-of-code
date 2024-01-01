-- https://adventofcode.com/2016/day/18
module Day18 (solve) where
import           AOC.Prelude hiding (get, fromList, toList)
import           AOC (aoc)
import           Data.Massiv.Array hiding (elem, map, sum, take)
import           Data.Massiv.Array.Unsafe (makeUnsafeStencil)
import           AOC.Parser (Parser, some)
import           AOC.List (count)

type Row = Array U Ix1 Bool -- False -> Safe, True -> Trapped

parser :: Parser Row
parser = fromList Seq <$> some trap where
    trap = False <$ "." <|> True <$ "^"

stencil :: Stencil Ix1 Bool Bool
stencil = makeUnsafeStencil (Sz 3) 0 \_ get ->
    let left = get (-1)
        center = get 0
        right = get 1
        n = fromEnum left + fromEnum center * 2 + fromEnum right * 4
    in n `elem` [3, 6, 1, 4]

step :: Row -> Row
step = compute . dropWindow . mapStencil (Fill False) stencil

solveFor :: Int -> Row -> Int
solveFor n = sum . map (count not . toList) . take n . iterate' step

solve :: Text -> IO ()
solve = aoc parser (solveFor 40) (solveFor 400_000)