-- https://adventofcode.com/2019/day/18
module Day18 (solve) where
import           AOC.Prelude
import           AOC (aoc)
import           AOC.Parser (Parser, choice, char, lowerChar, upperChar, sepEndBy1, some, eol)
import           Data.Char (toLower)
import           Data.Bits ((.|.), (.&.), shiftL, complement)
import           Data.List ((\\))
import qualified Data.HashMap.Strict as Map
import           AOC.MassivArray (Matrix, B, Comp(Seq), (!), (//), fromLists')
import qualified AOC.MassivArray as A
import qualified Data.Vector as V
import qualified Data.Vector.Instances ()
import           AOC.V2 (V2(..), adjacent, surrounding, toIx2)
import           AOC.List (flattenWithIndex', headMaybe)
import           AOC.Tuple (fst3)
import           AOC.Graph (bfsOn, dijkstra)

-- assume that the graph is acyclic
-- assume that each robot is in its own connected component for part 2

data Tile = Start | Empty | Wall | Key Keys | Door Keys deriving (Eq, Ord, Generic)
instance Hashable Tile

-- a graph is a hashamp whoses keys are the locations of keys and of the start
-- it associates to each location the locations of keys, the distance between them and the doors between them
type Graph = HashMap Tile [(Tile, Keys, Int)]
type Keys = Int

data SearchState = SearchState !(V.Vector Tile) !Keys deriving (Eq, Ord, Generic)
instance Hashable SearchState

isKey :: Tile -> Bool
isKey (Key _) = True
isKey _ = False

charToInt :: Char -> Int
charToInt c = 1 `shiftL` (ord c - ord 'a')

parser :: Parser (Matrix B Tile)
parser = fromLists' Seq <$> some tile `sepEndBy1` eol where
    tile = choice 
            [ Start <$ char '@'
            , Empty <$ char '.'
            , Wall <$ char '#'
            , Key . charToInt <$> lowerChar
            , Door . charToInt . toLower <$> upperChar
            ]

neighbors :: Matrix B Tile -> (V2 Int, Tile) -> [(V2 Int, Tile)]
neighbors grid (pos, _) = [ (pos', tile') 
                          | pos' <- adjacent pos
                          , let tile' = grid ! toIx2 pos'
                          , tile' /= Wall 
                          ]


neighbors1 :: V2 Int -> Matrix B Tile -> (V2 Int, Tile, Keys) -> [(V2 Int, Tile, Keys)]
neighbors1 start grid (pos, tile, doors)
    | pos /= start && isKey tile = []
    | otherwise = [ (pos', tile', doors .|. doors_ tile)
                  | pos' <- adjacent pos
                  , let tile' = grid ! toIx2 pos'
                  , tile' /= Wall 
                  ]
    where
    doors_ (Door d) = d
    doors_ _ = 0

mkGraph :: Matrix B Tile -> V2 Int -> Graph
mkGraph grid start = Map.fromList adjs where
    tiles = [ (pos, tile) 
            | (_, (pos, tile)) <- bfsOn fst (neighbors grid) (start, Start) 
            , isKey tile || tile == Start
            ]
    adjs = [ (tile, [ (tile', doors, dist) 
                    | (dist, (_, tile', doors)) <- drop 1 $ bfsOn fst3 (neighbors1 pos grid) (pos, tile, 0)
                    , isKey tile'
                    ]
              )
           | (pos, tile) <- tiles
           ]

neighbors2 :: Graph -> (Tile, Keys) -> [((Tile, Keys), Int)]
neighbors2 graph (v, keys) = do
    (tile, doors, cost) <- graph Map.! v
    case tile of
        Key k | (complement keys .&. doors) == 0 -> pure ((tile, k .|. keys), cost)
        _ -> []

part1 :: Matrix B Tile -> Maybe Int
part1 grid = do
    let allKeys = (1 `shiftL` length [() | Key _ <- A.toList grid]) - 1
    start <- headMaybe [ pos
                       | (pos, Start) <- flattenWithIndex' (A.toLists2 grid)
                       ]
    let graph = mkGraph grid start
    dijkstra (neighbors2 graph) (\(_, keys) -> keys == allKeys) (Start, 0)

neighbors3 :: [Graph] -> SearchState -> [(SearchState, Int)]
neighbors3 graphs (SearchState tiles keys) =
    concat $ zipWith3 go [0..] graphs (V.toList tiles) where
    go i graph tile =
        [ (SearchState tiles' keys', cost)
        | ((tile', keys'), cost) <- neighbors2 graph (tile, keys)
        , let tiles' = tiles V.// [(i, tile')]
        ]

part2 :: Matrix B Tile -> Maybe Int
part2 grid = do
    let allKeys = (1 `shiftL` length [() | Key _ <- A.toList grid]) - 1
    start <- headMaybe [ pos 
                       | (pos, Start) <- flattenWithIndex' (A.toLists2 grid)
                       ]
    let starts = surrounding start \\ adjacent start
    let newWalls = start : adjacent start
    let grid' = grid // ([(toIx2 p, Start) | p <- starts] ++ [(toIx2 p, Wall) | p <- newWalls])
    let graphs = map (mkGraph grid') starts
    let isGoal (SearchState _ keys) = keys == allKeys
    dijkstra (neighbors3 graphs) isGoal (SearchState (V.replicate 4 Start) 0)

solve :: Text -> IO ()
solve = aoc parser part1 part2
