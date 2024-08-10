-- https://adventofcode.com/2018/day/3
module Day03 (solve) where
import           AOC.Prelude hiding (toList)
import           AOC (aoc')
import           AOC.List (count, headMaybe)
import           AOC.Parser (Parser, sepEndBy1, eol, decimal, scanf)
import           Data.Massiv.Array (Array, U, Sz(..), Ix2(..), (!), toList)
import           Data.Massiv.Array.Mutable

parser :: Parser [(Int, Int, Int, Int, Int)]
parser = row `sepEndBy1` eol where
    row = [scanf|#{decimal} @ {decimal},{decimal}: {decimal}x{decimal}|]

precomp ::  [(Int, Int, Int, Int, Int)] -> ([(Int, Int, Int, Int, Int)], Array U Ix2 Int)
precomp instructions = (instructions, arr') where
    arr' = createArrayST_ @U (Sz2 1000 1000) \arr -> do
        for_ instructions \(_, x, y, w, h) ->
            for_ [x..x+w-1] \i ->
                for_ [y..y+h-1] \j ->
                    modify_ arr (pure . (+1)) (Ix2 i j)

part1 :: (a, Array U Ix2 Int) -> Int
part1 = count (>1) . toList . snd

isIntact ::Array U Ix2 Int ->  (Int, Int, Int, Int, Int) -> Maybe Int
isIntact arr (id_, x, y, w, h) =
    if null [() | i <- [x..x+w-1], j <- [y..y+h-1], (arr ! Ix2 i j) /= 1]
        then Just id_
        else Nothing

part2 :: ([(Int, Int, Int, Int, Int)], Array U Ix2 Int) -> Maybe Int
part2 (instructions, arr) = headMaybe $ mapMaybe (isIntact arr) instructions

solve :: Text -> IO ()
solve = aoc' parser (Just . precomp) part1 part2