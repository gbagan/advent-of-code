-- https://adventofcode.com/2019/day/2
module Day02 (solve) where
import           AOC.Prelude
import           AOC (aoc)
import           AOC.Parser (Parser, decimal, sepEndBy1)
import           Control.Monad.ST (runST)
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as VM

parser :: Parser (V.Vector Int)
parser = V.fromList <$> decimal `sepEndBy1` ","

runProgram :: Int -> Int -> V.Vector Int -> Int
runProgram noun verb v = runST do
    mv <- V.thaw v
    VM.write mv 1 noun
    VM.write mv 2 verb
    go mv 0
    where 
    go vec idx = do
        op <- VM.read vec idx
        case op of
            n | n == 1 || n == 2 -> do
                idx1 <- VM.read vec (idx+1)
                idx2 <- VM.read vec (idx+2)
                idx3 <- VM.read vec (idx+3)
                x <- VM.read vec idx1
                y <- VM.read vec idx2
                let res = if op == 1 then x + y else x * y
                VM.write vec idx3 res
                go vec (idx+4)
            99 -> VM.read vec 0
            _ -> error "run: invalid instruction"


part1 :: V.Vector Int -> Int 
part1 = runProgram 12 2

part2 :: V.Vector Int -> Maybe Int 
part2 vec = listToMaybe [ 100 * noun + verb
                        | noun <- [0..99]
                        , verb <- [0..99]
                        , runProgram noun verb vec == 19690720
                        ]

solve :: Text -> IO ()
solve = aoc parser part1 part2

-- 5130987
-- 190643