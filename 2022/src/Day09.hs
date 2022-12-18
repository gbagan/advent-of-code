-- https://adventofcode.com/2022/day/9
module Day09 (solve) where
import           RIO
import           RIO.List (scanl')
import           RIO.List.Partial (last)
import qualified RIO.Set as Set
import           Text.Megaparsec (sepEndBy1)
import           Text.Megaparsec.Char (char, eol, space)
import           Text.Megaparsec.Char.Lexer (decimal)
import           Util (Parser, aoc, clamp)

data Direction = L | R | U | D
data Knot = Knot !Int !Int deriving (Eq, Ord)
type Rope = [Knot]

parser :: Parser [Direction]
parser = concat <$> (line `sepEndBy1` eol) where
    line = flip replicate <$> direction <* space <*> decimal
    direction = L <$ char 'L'
            <|> R <$ char 'R'
            <|> U <$ char 'U'
            <|> D <$ char 'D'

moveKnot :: Knot -> Direction -> Knot
moveKnot (Knot x y) dir = case dir of
    L -> Knot (x-1) y
    R -> Knot (x+1) y
    D -> Knot x (y-1)
    U -> Knot x (y+1)

pullKnot :: Knot -> Knot -> Knot
pullKnot (Knot hx hy) (Knot tx ty)
        | Knot (hx-tx) (hy-ty) == Knot dx dy = Knot tx ty
        | otherwise = Knot (tx+dx) (ty+dy)
    where
    dx = clamp (-1, 1) (hx-tx)
    dy = clamp (-1, 1) (hy-ty)

moveRope :: Rope -> Direction -> Rope
moveRope [] _ = []
moveRope (k:ks) dir = scanl' pullKnot (moveKnot k dir) ks
    
solve' :: Int -> [Direction] -> Int
solve' n dirs = Set.size vis where
    (_, vis) = foldl' go (replicate n (Knot 0 0), Set.singleton (Knot 0 0)) dirs
    go (rope, visited) dir = (rope', visited') where
        rope' = moveRope rope dir
        visited' = Set.insert (last rope') visited

solve :: Text -> RIO env ()
solve = aoc parser (solve' 2) (solve' 10)