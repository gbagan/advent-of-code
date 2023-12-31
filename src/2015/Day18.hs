-- https://adventofcode.com/2015/day/18
module Day18 (solve) where
import           AOC.Prelude hiding (get, toList)
import           AOC (aoc)
import           Data.Massiv.Array hiding (elem)
import           Data.Massiv.Array.Unsafe (makeUnsafeStencil)
import           AOC.Parser (Parser, sepEndBy1, eol, some)
import           AOC.List (count)
import           AOC.Util (times)

type Grid = Array U Ix2 Int -- 0 off, 1 on, 2 fixed

parser :: Parser Grid
parser = fromLists' Seq <$> some bit `sepEndBy1` eol where
    bit = 0 <$ "." <|> 1 <$ "#"

stencil :: Stencil Ix2 Int Int
stencil = makeUnsafeStencil (Sz2 3 3) (0 :. 0) \_ get ->
    let n = count (/=0) [get (i :. j) | i <- [-1..1], j <- [-1..1]]
        v = get (0 :. 0)
    in if | v == 2 -> 2
          | n == 3 || v == 1 && n == 4 -> 1
          | otherwise -> 0

step :: Grid -> Grid
step = compute . dropWindow . mapStencil (Fill 0) stencil

part1 :: Grid -> Int
part1 = count (/=0) . toList . times 100 step

fixCorners :: Grid -> Grid
fixCorners grid = compute $ imap (\i v -> if i `elem` corners then 2 else v) grid
    where
    Sz2 h w = size grid
    corners = [0 :. 0, h-1 :. 0, 0 :. w-1, h-1 :. w-1]

part2 :: Grid -> Int
part2 = count (/=0) . toList . times 100 step . fixCorners

solve :: Text -> IO ()
solve = aoc parser part1 part2