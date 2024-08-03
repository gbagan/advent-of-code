-- https://adventofcode.com/2019/day/2
module Day02 (solve) where
import           AOC.Prelude
import           AOC (aoc)
import           AOC.Parser (Parser, decimal, sepEndBy1)
import qualified Data.Vector.Unboxed as V
import           AOC.IntCode (runProgram)

parser :: Parser (V.Vector Int)
parser = V.fromList <$> decimal `sepEndBy1` ","

run :: Int -> Int -> V.Vector Int -> Int 
run noun verb pgm = pgm' V.! 0 where 
    (pgm', _) = runProgram [] (pgm V.// [(1, noun), (2, verb)])

part1 :: V.Vector Int -> Int 
part1 = run 12 2 

part2 :: V.Vector Int -> Maybe Int 
part2 pgm = listToMaybe [ 100 * noun + verb
                        | noun <- [0..99]
                        , verb <- [0..99]
                        , run noun verb pgm == 19690720
                        ]

solve :: Text -> IO ()
solve = aoc parser part1 part2