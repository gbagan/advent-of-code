-- https://adventofcode.com/2020/day/20
module Day20 (solve) where

import           AOC.Prelude hiding (head, last, init, tail)
import           Data.List (head, last, init, tail)
import qualified Data.HashMap.Strict as Map
import           Data.Massiv.Array (Matrix, (!), (!?), U, B, BL, Comp(Seq), Sz(..), Ix2(..))
import qualified Data.Massiv.Array as A
import           AOC (aoc)
import           AOC.Parser (Parser, sepEndBy1, eol, decimal, some)
import           AOC.List (count, freqs', headMaybe)
import           AOC.Number (toDouble)
import           AOC.Tuple (thd3)
import           AOC.Util (binToInt)

type Grid = [[Bool]]
type Input = [(Int, Grid)]

intSqrt :: Int -> Int
intSqrt = floor . sqrt . toDouble

borderHash :: [Bool] -> Int
borderHash xs = 10000000 * min x y + max x y where
    x = binToInt xs
    y = binToInt (reverse xs)

parser :: Parser Input
parser = grid `sepEndBy1` eol where
    grid = do
        id_ <- "Tile " *> decimal <* ":" <* eol
        (id_,) <$> some tile `sepEndBy1` eol
    tile = False <$ "." <|> True <$ "#"

borders :: (Int, Grid) -> (Int, Grid, [Int])
borders (id_, grid) = (id_, grid, bds) where
    bds = fmap borderHash [head grid, last grid, head tgrid, last tgrid]
    tgrid = transpose grid

part1 :: Input -> Int
part1 grids = product corners where
    bds = fmap borders grids
    freqs = freqs' (concatMap thd3 bds)
    corners = [id_ | (id_, _, bds') <- bds, count (\b -> freqs Map.! b >= 2) bds' == 2]

seaMonster :: Grid
seaMonster = map (map (=='#')) [ "                  # "
                               , "#    ##    ##    ###"
                               , " #  #  #  #  #  #   "
                               ]

blackPositions :: Grid -> [(Int, Int)]
blackPositions l =
    [ (i, j)
    | (i, row) <- zip [0..] l
    , (j, True) <- zip [0..] row
    ]

orientations :: Grid -> [Grid]
orientations grid = do
    f1 <- [id, transpose]
    f2 <- [id, map reverse]
    f3 <- [id, reverse]
    pure . f1 . f2 $ f3 grid

doJigsaw :: [(Int, Grid)] -> (Int, Grid) -> Matrix BL (Int, Grid)
doJigsaw grids nwCorner = arr where
    n = intSqrt (length grids)
    arr = A.makeArray Seq (Sz2 n n) \(Ix2 row col) ->
        if | row == 0 && col == 0 -> nwCorner
           | row == 0 ->
                let (leftId, leftGrid) = arr ! Ix2 row (col-1)
                    leftBorder = map last leftGrid
                in head
                    [ (id_, orientation)
                    | (id_, grid) <- grids
                    , id_ /= leftId
                    , orientation <- orientations grid
                    , map head orientation == leftBorder
                    ]
           | otherwise ->
                let (topId, topGrid) = arr ! Ix2 (row-1) col
                    topBorder = last topGrid
                in head
                    [ (id_, orientation)
                    | (id_, grid) <- grids
                    , id_ /= topId
                    , orientation <- orientations grid
                    , head orientation == topBorder
                    ]

drawPixels :: Matrix BL (Int, Grid) -> Matrix U Bool
drawPixels arr = pixels where
    arr' = A.compute @B $ A.map
            (A.fromLists' @U Seq . map (tail . init) . tail . init . snd)
            arr
    Sz2 h _ = A.size arr'
    Sz2 h' _ = A.size (arr' ! Ix2 0 0)
    pixels = A.makeArray Seq (Sz2 (h*h') (h*h')) \(Ix2 i j) ->
        arr' ! Ix2 (i `div` h') (j `div` h') ! Ix2 (i `mod` h') (j `mod` h')

roughness :: Matrix U Bool -> Int
roughness pixels = nbBlacks - nbSeaMonsters * seaMonsterSize where
    nbSeaMonsters = length
             [ ()
             | p <- orientations seaMonster
             , let monsterPositions = blackPositions p
             , x <- [0..h-1]
             , y <- [0..h-1]
             , all (\(x', y') -> (pixels !? Ix2 (x+x') (y+y')) == Just True) monsterPositions
             ]
    seaMonsterSize = length (blackPositions seaMonster)
    nbBlacks = count id (A.toList pixels)
    Sz2 h _ = A.size pixels

part2 :: Input -> Maybe Int
part2 grids = do
    let bds = fmap borders grids
    let freqs = freqs' (concatMap thd3 bds)
    (cornerId, corner) <- headMaybe [ (id_, grid)
                            | (id_, grid, bds') <- bds
                            , count (\b -> freqs Map.! b >= 2) bds' == 2
                            ]
    orientedCorner <- orientations corner & find \grid ->
        freqs Map.! borderHash (head grid) == 1
        && freqs Map.! (borderHash . head $ transpose grid) == 1
    let jigsaw = doJigsaw grids (cornerId, orientedCorner)
    let pixels = drawPixels jigsaw
    pure $ roughness pixels

solve :: Text -> IO ()
solve = aoc parser part1 part2