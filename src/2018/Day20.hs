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

data Expr = Singleton (V2 Int) | Sequence [Expr] | Disjunction [Expr]

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
mkGrid = Set.insert origin . fst . go (Set.singleton origin) where
    go startingRooms (Singleton dir) = (visited, endRooms) where
        doors = Set.map (+dir) startingRooms
        endRooms = Set.map (+dir) doors
        visited = doors `Set.union` endRooms
    go startingRooms (Disjunction exprs) = (Set.unions visited, Set.unions endRooms) where
        res = map (go startingRooms) exprs
        visited = map fst res
        endRooms = map snd res
    go startingRooms (Sequence exprs) = foldl'
                                            (\(v, s) -> first (Set.union v) . go s)
                                            (Set.empty, startingRooms)
                                            exprs

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