-- https://adventofcode.com/2023/day/21
module Day21 (solve) where
import           AOC.Prelude
import           Data.Massiv.Array (Matrix, (!), fromLists', size, U, Comp(Seq), Sz(..))
import           AOC (aoc')
import           AOC.Parser (Parser, sepEndBy1, some, eol)
import           AOC.List (count, flattenWithIndex', headMaybe)
import           AOC.V2 (V2(..), adjacent, toIx2)
import           AOC.Graph (bfs)

data Tile = Garden | Rock | Start deriving (Eq)
type Grid = Matrix U Bool

parser :: Parser [[Tile]]
parser = some tile `sepEndBy1` eol where
    tile = Garden <$ "." <|> Rock <$ "#" <|> Start <$ "S"

precomp :: [[Tile]] -> Maybe (Grid, V2 Int)
precomp tiles = do
    start <- headMaybe [pos | (pos, Start) <- flattenWithIndex' tiles]
    let matrix = fromLists' Seq $ map (map (==Rock)) tiles
    pure (matrix, start)

neighbors :: Grid -> V2 Int -> [V2 Int]
neighbors grid = filter (not . (grid !) . toIx2 . mod2) . adjacent where
    Sz2 h w = size grid
    mod2 (V2 r c) = V2 (r `mod` h) (c `mod` w)

-- count the number of positions reachable from start with exactly n movements
-- use the result of a bfs as input
nbReachable :: Int -> [(Int, a)] -> Integer
nbReachable n = fromIntegral
                . count (\y -> even (y - n))
                . takeWhile (<=n) 
                . map fst

part1 :: (Grid, V2 Int) -> Integer
part1 (grid, start) = nbReachable 64 $ bfs (neighbors grid) start

-- given a quadratic sequence with first terms u0, u1, u2,  compute u_n
quadraticSequence :: Integer -> Integer -> Integer -> Integer -> Integer
quadraticSequence u0 u1 u2 n = u0 + n * u0' + n * (n-1) * u0'' `div` 2
    where
    u0' = u1 - u0
    u1' = u2 - u1
    u0'' = u1' - u0'

part2 :: (Grid, V2 Int) -> Integer
part2 (grid, start) = result where
    nbSteps = 26_501_365
    Sz2 h _ = size grid
    r = nbSteps `mod` h
    bfsResult = bfs (neighbors grid) start
    u0 = nbReachable r bfsResult
    u1 = nbReachable (r+h) bfsResult
    u2 = nbReachable (r+2*h) bfsResult
    result = quadraticSequence u0 u1 u2 . fromIntegral $ nbSteps `div` h

solve :: Text -> IO ()
solve = aoc' parser precomp part1 part2