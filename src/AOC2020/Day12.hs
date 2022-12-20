-- https://adventofcode.com/2020/day/12
module AOC2020.Day12 (solve) where
import           RIO
import           Data.List (iterate')
import           RIO.List.Partial ((!!))
import           Text.Megaparsec (sepEndBy1)
import           Text.Megaparsec.Char (char, eol)
import           Text.Megaparsec.Char.Lexer (decimal)
import           Linear.V2 (V2(..), perp)
import           Util (Parser, aoc)

data Instr = N !Int | S !Int | W !Int | E !Int | F !Int | L !Int | R !Int

parser :: Parser [Instr]
parser = instr `sepEndBy1` eol where
    instr = instr' <*> decimal
    instr' = N <$ char 'N' <|> S <$ char 'S'
         <|> W <$ char 'W' <|> E <$ char 'E'
         <|> L <$ char 'L' <|> R <$ char 'R'
         <|> F <$ char 'F'

part1 :: [Instr] -> Int
part1 xs = abs a + abs b where
    (V2 a b, _) = foldl' go (V2 0 0, V2 1 0) xs
    go (pos, dir) = \case
        N n -> (pos + V2 0 n, dir)
        S n -> (pos - V2 0 n, dir)
        W n -> (pos - V2 n 0, dir)
        E n -> (pos + V2 n 0, dir)
        F n -> (pos + fmap (*n) dir, dir)
        L n -> (pos, iterate' perp dir !! (n `div` 90))
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
        L n -> (pos, iterate' perp wpos !! (n `div` 90))
        R n -> go (pos, wpos) (L $ 360-n)


solve :: MonadIO m => Text -> m ()
solve = aoc parser part1 part2