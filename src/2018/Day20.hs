-- https://adventofcode.com/2018/day/20
module Day20 (solve) where
import           AOC.Prelude hiding (last, sequence)
import           AOC (aoc_)
import           Data.List (last)
import qualified Data.HashSet as Set
import           AOC.Graph (bfs)
import           AOC.List (count)
import           AOC.Parser (Parser, between, char, choice, sepBy1, many)
import           AOC.V2 (V2, origin, west, east, north, south, adjacent)

data Expr = Sequence [Expr] | Singleton (V2 Int) | Disjunction [Expr]

parser :: Parser Expr
parser = "^" *> sequence <* "$" where
    sequence = Sequence <$> many atom
    atom = choice 
            [ Singleton north <$ char 'N'
            , Singleton south <$ char 'S'
            , Singleton west <$ char 'W'
            , Singleton east <$ char 'E'
            , Disjunction <$> between (char '(') (char ')') (sequence `sepBy1` char '|')
            ]

mkGrid :: Expr -> HashSet (V2 Int)
mkGrid = fst . go (Set.singleton origin, Set.singleton origin) where
    go (visited, endPoints) (Singleton dir) = (visited', endPoints'') where
        endPoints' = Set.map (+dir) endPoints
        endPoints'' = Set.map (+dir) endPoints'
        visited' = visited `Set.union` endPoints' `Set.union` endPoints''
    go (visited, endPoints) (Disjunction exprs) = (Set.unions (visited:visited'), Set.unions endPoints') where
        res = map (go (Set.empty, endPoints)) exprs
        visited' = map fst res
        endPoints' = map snd res
    go (visited, endPoints) (Sequence exprs) = foldl' go (visited, endPoints) exprs

isRoom :: V2 Int -> Bool
isRoom = even . sum

solve' :: Expr -> Maybe (Int, Int)
solve' expr = Just (p1, p2) where
    grid = mkGrid expr
    neighbors v = [u | u <- adjacent v, u `Set.member` grid]
    distances = [d `div` 2 | (d, p) <- bfs neighbors origin, isRoom p]
    p1 = last distances
    p2 = count (>= 1000) distances

solve :: Text -> IO ()
solve = aoc_ parser solve'