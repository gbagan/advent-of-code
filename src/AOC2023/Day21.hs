-- https://adventofcode.com/2023/day/21
module Day21 (solve) where
import           AOC.Prelude hiding (head, last)
import           Data.Massiv.Array (Matrix, (!), fromLists', size, U, Comp(Seq), Sz(..))
import           AOC (aoc')
import           AOC.Parser (Parser, sepEndBy1, some, eol)
import           AOC.List (count, flattenWithIndex)
import           AOC.V2 (V2(..), adjacent, toIx2)
import           AOC.Search (bfs)

data Tile = Garden | Rock | Start deriving (Eq)
type Grid = Matrix U Bool

parser :: Parser [[Tile]]
parser = some tile `sepEndBy1` eol where
    tile = Garden <$ "." <|> Rock <$ "#" <|> Start <$ "S"

precomp :: [[Tile]] -> Maybe (Grid, V2 Int)
precomp tiles = do
    start <- listToMaybe [V2 i j | (i, j, Start) <- flattenWithIndex tiles]
    let tiles' = map (map (==Rock)) tiles
    let matrix = fromLists' Seq tiles'
    pure (matrix, start)

nbors :: Grid -> V2 Int -> [V2 Int]
nbors grid = filter (not . (grid !) . toIx2 . mod2) . adjacent where
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
part1 (grid, start) = nbReachable 64 $ bfs (nbors grid) start

-- given a quadratic sequence with first terms u0, u1, u0,  compute u_n
quadraticSequence :: Integer -> Integer -> Integer -> Integer -> Integer
quadraticSequence u0 u1 u2 n = u0 + n * d1 + n * (n-1) * d' `div` 2
    where
    d1 = u1 - u0
    d2 = u2 - u1
    d' = d2 - d1

part2 :: (Grid, V2 Int) -> Integer
part2 (grid, start) = result where
    nbSteps = 26_501_365
    Sz2 h _ = size grid
    r = nbSteps `mod` h
    bfsResult = bfs (nbors grid) start
    u0 = nbReachable r bfsResult
    u1 = nbReachable (r+h) bfsResult
    u2 = nbReachable (r+2*h) bfsResult
    result = quadraticSequence u0 u1 u2 (fromIntegral $ nbSteps `div` h)

solve :: Text -> IO ()
solve = aoc' parser precomp part1 part2