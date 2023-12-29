-- https://adventofcode.com/2015/day/24
module Day24 (solve) where
import           AOC.Prelude hiding (fromList, product, sum)
import           AOC (aoc)
import           AOC.Parser (Parser, decimal, eol, sepEndBy1)
import           Data.Massiv.Array
import           AOC.List (minimumMaybe)
-- import           AOC.Number (toInteger)

parser :: Parser (Array U Ix1 Int)
parser = fromList Seq <$> decimal `sepEndBy1` eol

solveFor :: Int -> Array U Ix1 Int -> Maybe Integer
solveFor k xs = arr ! Ix2 m n where
    Sz m = size xs
    n = sum xs `div` k
    arr = makeArray @BL Seq (Sz2 (m+1) (n+1)) \(Ix2 i j) ->
        if | i == 0 && j == 0 -> Just 1
           | i == 0 -> Nothing
           | otherwise -> let v = xs ! i-1 in
                        minimumMaybe $ catMaybes
                            [ (* toInteger v) <$> join (arr !? Ix2 (i-1) (j - v))
                            , join $ arr !? Ix2 (i-1) j
                            ]

solve :: Text -> IO ()
solve = aoc parser (solveFor 3) (solveFor 4)