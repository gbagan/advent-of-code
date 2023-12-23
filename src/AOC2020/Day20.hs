-- https://adventofcode.com/2020/day/20
module Day20 (solve) where

import           AOC.Prelude hiding (head, last)
import           Data.List (head, last)
import qualified Data.HashMap.Strict as Map
import           AOC (aoc)
import           AOC.Parser (Parser, sepEndBy1, eol, decimal, some)
import           AOC.List (count, freqs')

type Grid = [[Bool]]
type Input = [(Int, Grid)]

parser :: Parser Input
parser = grid `sepEndBy1` eol where
    grid = do
        id_ <- "Tile " *> decimal <* ":" <* eol
        (id_,) <$> some tile `sepEndBy1` eol
    tile = False <$ "." <|> True <$ "#"

borders :: (Int, Grid) -> (Int, [([Bool], [Bool])])
borders (id_, grid) = (id_, bds) where
    bds = [sortTuple (b, reverse b) | b <- [head grid, last grid, head tgrid, last tgrid]]
    tgrid = transpose grid 
    sortTuple (x, y) = if x < y then (x, y) else (y, x)

part1 :: Input -> Int
part1 grids = product corners where
    bds = fmap borders grids
    freqs = freqs' (concatMap snd bds) 
    corners = [id_ | (id_, bds') <- bds, count (\b -> freqs Map.! b >= 2) bds' == 2] 

part2 :: Input -> Int
part2 _ = 0

solve :: Text -> IO ()
solve = aoc parser part1 part2