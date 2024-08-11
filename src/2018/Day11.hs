-- https://adventofcode.com/2018/day/11
module Day11 (solve) where
import           AOC.Prelude
import           AOC (aoc')
import           AOC.Parser (decimal)
import           AOC.List (maximumOn)
import           Data.Massiv.Array (Matrix, (!), makeArray, BL, Comp(Seq), Ix2(..), Sz(..))

dim :: Int
dim = 300

powerLevel :: Int -> (Int, Int) -> Int
powerLevel serialNumber (x, y) = (pl `div` 100) `mod` 10 - 5 where
    rackId = x + 10
    pl = (rackId * y + serialNumber) * rackId

partialSums :: Int -> Matrix BL Int
partialSums serialNumber = sums where
    sums = makeArray @BL Seq (Sz2 (dim+1) (dim+1)) \(Ix2 i j) ->
        if i == 0 || j == 0
            then 0
            else (sums ! Ix2 (i-1) j) + (sums ! Ix2 i (j-1)) - (sums ! Ix2 (i-1) (j-1)) + powerLevel serialNumber (i, j)

rectangleSum :: Matrix BL Int -> Int -> Int -> Int -> Int -> Int
rectangleSum sums x y width height = (sums ! Ix2 (x+width-1) (y+height-1))
                                        + (sums ! Ix2 (x-1) (y-1))
                                        - (sums ! Ix2 (x-1) (y+height-1))
                                        - (sums ! Ix2 (x+width-1) (y-1))

findMaxSquare :: Matrix BL Int -> Int -> Int -> (Int, Int, Int)
findMaxSquare sums min_ max_ =
        maximumOn (\(x, y, size) -> rectangleSum sums x y size size)
            [ (x, y, size)
            | size <- [min_..max_]
            , x <- [1 .. dim-size+1]
            , y <- [1 .. dim-size+1]
            ]

part1 :: Matrix BL Int -> (Int, Int)
part1 sums = (x, y) where
    (x, y, _) = findMaxSquare sums 3 3

part2 :: Matrix BL Int -> (Int, Int, Int)
part2 sums = findMaxSquare sums 1 dim
  
solve :: Text -> IO ()
solve = aoc' decimal (Just . partialSums) part1 part2