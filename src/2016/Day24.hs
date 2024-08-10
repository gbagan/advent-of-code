-- https://adventofcode.com/2017/day/24
module Day24 (solve) where
import           AOC.Prelude hiding (head)
import           AOC (aoc')
import           AOC.Parser (Parser, choice, char, sepEndBy1, eol, some)
import           AOC.List (flattenWithIndex', minimumDef)
import           AOC.V2 (adjacent, toIx2)
import           AOC.Graph (bfs)
import           Data.Massiv.Array ((!), U, Comp(Seq))
import qualified Data.Massiv.Array as A
import qualified Data.IntMap.Strict as Map
import qualified Data.IntSet as Set
import           Data.Char (isDigit, digitToInt)

type Input = IntMap [(Int, Int)]

parser :: Parser [[Char]]
parser = some tile `sepEndBy1` eol where
    tile = choice (map char ".#0123456789")

mkInput :: [[Char]] -> Input
mkInput grid = Map.fromList distances where
    grid' = A.fromLists' @U Seq grid
    distances = [ (digitToInt n, distancesFrom pos)
                | (pos, n) <- flattenWithIndex' grid
                , isDigit n
                ]
    distancesFrom start = mapMaybe (\(dist, pos) ->
                let tile = grid' ! toIx2 pos in
                if isDigit tile
                    then Just (digitToInt tile, dist)
                    else Nothing
                ) (bfs nbors start)
    nbors pos = [p | p <- adjacent pos, (grid' ! toIx2 p) /= '#']

tsp :: Bool -> IntMap [(Int, Int)] -> Int
tsp returnToZero dists = go (Set.singleton 0) 0 0 where
    n = Map.size dists
    distanceToZero = Map.fromList (dists Map.! 0)
    go visited current len
        | Set.size visited == n = len + if returnToZero then distanceToZero Map.! current else 0
        | otherwise = minimumDef 0 [ go (Set.insert next visited) next $! len+len'
                                   | (next, len') <- dists Map.! current
                                   , not $ next `Set.member` visited
                                   ]

solve :: Text -> IO ()
solve = aoc' parser (Just . mkInput) (tsp False) (tsp True)
