-- https://adventofcode.com/2020/day/12
module Day12 (solve) where
import           AOC.Prelude
import           AOC.V2 (V2(..), north, south, east, west, origin, turnLeft)
import           AOC (aoc)
import           AOC.Parser (Parser, choice, sepEndBy1, eol, decimal)
import           AOC.Util (times)

data Instr = N !Int | S !Int | W !Int | E !Int | F !Int | L !Int | R !Int

parser :: Parser [Instr]
parser = instr `sepEndBy1` eol where
    instr = instr' <*> decimal
    instr' = choice [N <$ "N",  S <$ "S", W <$ "W", E <$ "E", L <$ "L", R <$ "R", F <$ "F"]

part1 :: [Instr] -> Int
part1 xs = sum (fmap abs p) where
    (p, _) = foldl' go (origin, east) xs
    go (pos, dir) = \case
        N n -> (pos + fmap (*n) north, dir)
        S n -> (pos + fmap (*n) south, dir)
        W n -> (pos + fmap (*n) west, dir)
        E n -> (pos + fmap (*n) east, dir)
        F n -> (pos + fmap (*n) dir, dir)
        L n -> (pos, times (n `div` 90) turnLeft dir)
        R n -> go (pos, dir) (L $ 360-n)

part2 :: [Instr] -> Int
part2 xs = sum (fmap abs p) where
    (p, _) = foldl' go (origin, V2 (-1) 10) xs
    go (pos, wpos) = \case
        N n -> (pos, wpos + fmap (*n) north)
        S n -> (pos, wpos + fmap (*n) south)
        W n -> (pos, wpos + fmap (*n) west)
        E n -> (pos, wpos + fmap (*n) east)
        F n -> (pos + fmap (*n) wpos, wpos)
        L n -> (pos, times (n `div` 90) turnLeft wpos)
        R n -> go (pos, wpos) (L $ 360-n)


solve :: Text -> IO ()
solve = aoc parser part1 part2