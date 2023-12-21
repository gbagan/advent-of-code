-- https://adventofcode.com/2020/day/12
module AOC2020.Day12 (solve) where
import           AOC.Prelude
import           AOC.V2 (V2(..), perp)
import           AOC (aoc)
import           AOC.Parser (Parser, choice, sepEndBy1, eol, decimal)
import           AOC.Util (times)

data Instr = N !Int | S !Int | W !Int | E !Int | F !Int | L !Int | R !Int

parser :: Parser [Instr]
parser = instr `sepEndBy1` eol where
    instr = instr' <*> decimal
    instr' = choice [N <$ "N",  S <$ "S", W <$ "W", E <$ "E", L <$ "L", R <$ "R", F <$ "F"]

part1 :: [Instr] -> Int
part1 xs = abs a + abs b where
    (V2 a b, _) = foldl' go (V2 0 0, V2 1 0) xs
    go (pos, dir) = \case
        N n -> (pos + V2 0 n, dir)
        S n -> (pos - V2 0 n, dir)
        W n -> (pos - V2 n 0, dir)
        E n -> (pos + V2 n 0, dir)
        F n -> (pos + fmap (*n) dir, dir)
        L n -> (pos, times (n `div` 90) perp dir)
        R n -> go (pos, dir) (L $ 360-n)

part2 :: [Instr] -> Int
part2 xs = abs a + abs b where
    (V2 a b, _) = foldl' go (V2 0 0, V2 10 1) xs
    go (pos, wpos) = \case
        N n -> (pos, wpos + V2 0 n)
        S n -> (pos, wpos - V2 0 n)
        W n -> (pos, wpos - V2 n 0)
        E n -> (pos, wpos + V2 n 0)
        F n -> (pos + fmap (*n) wpos, wpos)
        L n -> (pos, times (n `div` 90) perp wpos)
        R n -> go (pos, wpos) (L $ 360-n)


solve :: Text -> IO ()
solve = aoc parser part1 part2