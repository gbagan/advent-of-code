-- https://adventofcode.com/2019/day/2
module Day02 (solve) where
import           AOC.Prelude hiding (head, get)
import           AOC (aoc)
import           AOC.Parser (Parser, signedDecimal, sepBy1)
import           AOC.IntCode (Effect(..), runEffect, newMachine, get , set)

parser :: Parser [Int]
parser = signedDecimal `sepBy1` ","

run :: Int -> Int -> [Int] -> Int
run noun verb = go . runEffect . set 2 verb . set 1 noun . newMachine where
    go = \case
        Halt machine -> get 0 machine
        Input _ -> error "runProgram_ does not manage inputs"
        Output _ _ -> error "runProgram_ does not manage outputs"

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