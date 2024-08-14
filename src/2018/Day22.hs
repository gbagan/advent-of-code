-- https://adventofcode.com/2018/day/22
module Day22 (solve) where
import           AOC.Prelude hiding (map)
import           AOC (aoc)
import           Data.Massiv.Array as A hiding (product, toIx2, sum)
import           AOC.Parser (Parser, eol, decimal, scanf)
import           AOC.V2 (V2(..), origin, manhattan, adjacent, toIx2)
import           AOC.Graph (astar)

data Node = Node (V2 Int) Int deriving (Eq, Ord, Generic)
instance Hashable Node

parser :: Parser (Int, V2 Int)
parser = do
    depth <- "depth: " *> decimal <* eol
    (targetX, targetY) <- [scanf|target: {decimal},{decimal}|]
    pure (depth, V2 targetY targetX)

riskFn :: Int -> V2 Int -> V2 Int -> Int
riskFn depth dest@(V2 y x) = \pos -> risk ! toIx2 pos where
    destIdx = toIx2 dest
    erosion = makeArray @BL Seq (Sz2 (y+200) (x+200)) \case
        (0 :. 0)                      -> 0
        (j :. 0)                      -> (j * 48271 + depth) `mod` 20183
        (0 :. i)                      -> (i * 16807 + depth) `mod` 20183
        idx@(j :. i) | idx == destIdx -> 0
            | otherwise   -> ((erosion ! (j-1:.i)) * (erosion ! (j:.i-1)) + depth) `mod` 20183
    risk = compute @U $ map (`mod` 3) erosion

part1 :: (Int, V2 Int) -> Int
part1 (depth, dest@(V2 y x)) = sum [risk (V2 j i) | i <- [0..x], j <- [0..y]] where
    risk = riskFn depth dest

part2 :: (Int, V2 Int) -> Maybe Int
part2 (depth, dest) = astar (Node origin torch) (== Node dest torch) neighbors heuristic where
    torch = 1
    risk = riskFn depth dest
    neighbors (Node pos tool) = [ (Node pos' tool, 1)
                                | pos'@(V2 y x) <- adjacent pos
                                , x >= 0 && y >= 0
                                , risk pos' /= tool
                                ] ++ 
                                [ (Node pos tool', 7)
                                | tool' <- [0..2]
                                , tool' /= tool && tool' /= risk pos
                                ]
    heuristic (Node pos tool) = manhattan pos dest + if tool == torch then 0 else 7

solve :: Text -> IO ()
solve = aoc parser part1 part2