-- https://adventofcode.com/2018/day/9
module Day09 (solve) where
import           AOC.Prelude hiding (round)
import           AOC (aoc)
import           AOC.Parser (Parser, decimal, scanf)
import qualified AOC.Sequence as Seq
import           AOC.Sequence (Seq(..))
import           Data.IntMap as Map
import           Data.List (maximum)

parser :: Parser (Int, Int)
parser = [scanf|{decimal} players; last marble is worth {decimal} points|]

simulate :: Int -> Int -> Int
simulate nbPlayers nbMarbles = go 1 Map.empty (Seq.singleton 0) where
    go round scores circle
        | round > nbMarbles = maximum scores
        | round `mod` 23 == 0 =
            case Seq.rotate (-7) circle of
                Seq.Empty -> error "simulate"
                removed :<| circle' -> go (round+1) scores' circle' where 
                    scores' = Map.insertWith (+) (round `mod` nbPlayers) (round + removed) scores
        | otherwise = go (round+1) scores (round :<| Seq.rotate 2 circle)

part1 :: (Int, Int) -> Int
part1 = uncurry simulate

part2 :: (Int, Int) -> Int
part2 (nbPlayers, nbMarbles ) = simulate nbPlayers (100*nbMarbles)

solve :: Text -> IO ()
solve = aoc parser part1 part2