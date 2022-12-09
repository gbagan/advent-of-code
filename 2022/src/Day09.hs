-- https://adventofcode.com/2022/day/9
module Day09 (solve) where
import           RIO
import           RIO.List.Partial (last)
import qualified RIO.Set as Set
import           Data.Ord (clamp)
import           Text.Megaparsec (sepEndBy1)
import           Text.Megaparsec.Char (char, eol, space)
import           Text.Megaparsec.Char.Lexer (decimal)
import           Util (Parser, aoc)

data Direction = L | R | U | D
type Knot = (Int, Int)
type Rope = [Knot]

parser :: Parser [Direction]
parser = concat <$> (line `sepEndBy1` eol) where
    line = flip replicate <$> direction <* space <*> decimal
    direction = L <$ char 'L'
            <|> R <$ char 'R'
            <|> U <$ char 'U'
            <|> D <$ char 'D'

moveKnot :: Knot -> Direction -> Knot
moveKnot (x, y) dir = case dir of
    L -> (x-1, y)
    R -> (x+1, y)
    D -> (x, y-1)
    U -> (x, y+1)

followFront :: Knot -> Knot -> Knot
followFront (x, y) (x', y')
        | (x'-x, y'-y) == (dx, dy) = (x, y)
        | otherwise = (x+dx, y+dy)
    where
    dx = clamp (-1, 1) (x'-x)
    dy = clamp (-1, 1) (y'-y)

moveRope :: Rope -> Direction -> Rope
moveRope [] _ = []
moveRope (k:ks) dir = k' : go k' ks where
    k' = moveKnot k dir
    go _ [] = []
    go y (y':ys) = y'' : go y'' ys where y'' = followFront y' y

solve' :: Int -> [Direction] -> Int
solve' n dirs = Set.size vis where
    (_, vis) = foldl' go (replicate n (0 :: Int, 0 :: Int), Set.singleton (0, 0)) dirs
    go (rope, visited) dir = (rope', visited') where
        rope' = moveRope rope dir
        visited' = Set.insert (last rope') visited

solve :: (HasLogFunc env) => Text -> RIO env ()
solve = aoc parser (solve' 2) (solve' 10)