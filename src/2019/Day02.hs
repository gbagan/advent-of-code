-- https://adventofcode.com/2019/day/2
module Day02 (solve) where
import           AOC.Prelude hiding (head)
import           AOC (aoc)
import           AOC.Parser (Parser, decimal, sepEndBy1)
import           AOC.IntCode (runProgram_)
import           Data.List (head)

parser :: Parser [Int]
parser = decimal `sepEndBy1` ","

run :: Int -> Int -> [Int] -> Int 
run noun verb pgm = head $ runProgram_ pgm' where
    x = head pgm
    pgm' = x : noun : verb : drop 3 pgm

part1 :: [Int] -> Int 
part1 = run 12 2 

part2 :: [Int] -> Maybe Int 
part2 pgm = listToMaybe [ 100 * noun + verb
                        | noun <- [0..99]
                        , verb <- [0..99]
                        , run noun verb pgm == 19690720
                        ]

solve :: Text -> IO ()
solve = aoc parser part1 part2