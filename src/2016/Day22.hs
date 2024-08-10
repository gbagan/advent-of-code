-- https://adventofcode.com/2016/day/22
module Day22 (solve) where
import           AOC.Prelude hiding (State, head)
import           Data.List (head)
import           AOC (aoc)
import           AOC.Parser (Parser, decimal, eol, scanf, hspace, sepEndBy1, skipLine)
import           AOC.List (groupOn)
import           AOC.V2 (V2(..), manhattan, origin, adjacent, toIx2)
import           Data.Massiv.Array hiding (map, dropWhile, toIx2)
import           AOC.Graph (astar)

data Node = Node { _used, _avail :: !Int}
type Input = [(V2 Int, Node)]

data State = State { _goal, _hole :: !(V2 Int) } deriving (Eq, Ord, Generic)
instance Hashable State

parser :: Parser Input
parser = skipLine *> skipLine *> node `sepEndBy1` eol where
    node = do
        (x, y, _, used, avail, _) <- [scanf|/dev/grid/node-x{d}-y{d}{d}T{d}T{d}T{d}%|]
        pure (V2 x y, Node used avail)
    d = hspace *> decimal

part1 :: Input -> Int
part1 nodes = aux useds avails len 0 where
    useds = dropWhile (==0) . sort $ map (_used . snd) nodes
    avails = sort $ map (_avail . snd) nodes
    len = length avails
    aux [] _ _ total = total
    aux _ [] _ total = total
    aux (u:us) (a:as) l total | u <= a    = aux us (a:as) l $! total + l 
                              | otherwise = aux (u:us) as (l-1) $! total 


findHole :: Input -> V2 Int
findHole input = head [ p | (p,node) <- input, _used node == 0 ]

inputToMat :: Input -> Matrix B Bool
inputToMat input = fromLists' Seq . map (map \(_, Node used avail) -> used + avail <= 120) 
                        $ groupOn (\(V2 x _, _) -> x) input       

neighbors :: Matrix B Bool -> State -> [(State, Int)]
neighbors grid (State goal hole) =
    [ (State goal' hole', 1)
    | hole' <- adjacent hole
    , (grid !? toIx2 hole') == Just True
    , let goal' = if hole' == goal then hole else goal
    ]

heuristic :: State -> Int
heuristic (State goal hole) = 5 * (manhattan goal origin - 1) + manhattan goal hole

isDest :: State -> Bool
isDest (State goal _) = goal == origin

part2 :: Input -> Maybe Int
part2 input = astar (State goal hole) isDest (neighbors mat) heuristic where
    mat = inputToMat input
    Sz2 w _ = size mat
    goal = V2 (w-1) 0
    hole = findHole input

solve :: Text -> IO ()
solve = aoc parser part1 part2