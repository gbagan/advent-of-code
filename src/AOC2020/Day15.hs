-- https://adventofcode.com/2020/day/15
module Day15 (solve) where
import           AOC.Prelude hiding (init, last, round)
import           Data.List (init, last)
import           Control.Monad.ST (ST, runST)
import           AOC (aoc)
import           AOC.Parser (Parser, sepBy1, decimal)
import qualified Data.Vector.Unboxed.Mutable as V

parser :: Parser [Word32]
parser = decimal `sepBy1` ","

solveFor :: Word32 -> [Word32] -> Word32
solveFor n xs = runST do
    arr <- V.replicate (fromIntegral n) 0
    zipWithM_ (V.write arr) (map fromIntegral (init xs)) [1..]
    go arr n (genericLength xs) (last xs)

go :: V.MVector s Word32 -> Word32 -> Word32 -> Word32 -> ST s Word32
go arr n round val
    | round == n = pure $! val
    | otherwise = do
        prevRound <- V.exchange arr (fromIntegral val) round
        go arr n (round+1) (if prevRound == 0 then 0 else round-prevRound)

solve :: Text -> IO ()
solve = aoc parser (solveFor 2020) (solveFor 30_000_000)