-- https://adventofcode.com/2019/day/18
module Day18 (solve) where
import           AOC.Prelude
import           AOC (aoc)
import           AOC.Parser (Parser, choice, char, lowerChar, upperChar, sepEndBy1, some, eol)
import           Data.Char (toLower)
import           Data.Bits ((.|.), (.&.), shiftL)
import           Data.List ((\\))
import qualified Data.HashMap.Strict as Map
import           AOC.MassivArray (Matrix, B, Comp(Seq), (!), (//), fromLists')
import qualified AOC.MassivArray as A
import qualified Data.Vector as V
import qualified Data.Vector.Instances ()
import           AOC.V2 (V2(..), adjacent, surrounding, toIx2)
import           AOC.List (flattenWithIndex)
import           AOC.Graph (bfsOn, dijkstra)


data Tile = Start | Empty | Wall | Key Keys | Door Keys deriving (Eq, Ord, Generic)
instance Hashable Tile

type Graph = HashMap Tile [(Tile, Int)]
type Keys = Int
data SearchState = SearchState !(V.Vector Tile) !Keys deriving (Eq, Ord, Generic)
instance Hashable SearchState

isInterestingTile :: Tile -> Bool
isInterestingTile Start = True
isInterestingTile (Key _) = True
isInterestingTile (Door _) = True
isInterestingTile _ = False

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

mkGraph :: Matrix B Tile -> V2 Int -> Graph
mkGraph grid start = Map.fromList adjs where
    interestingTiles = [ (pos, tile) 
                       | (_, (pos, tile)) <- bfsOn fst (neighbors grid) (start, Start) 
                       , isInterestingTile tile
                       ]
    adjs = [ (tile, [ (tile', w) 
                    | (w, (_, tile')) <- drop 1 $ bfsOn fst (neighbors1 pos grid) (pos, tile)
                    , isInterestingTile tile'
                    ]
              )
           | (pos, tile) <- interestingTiles
           ]

neighbors :: Matrix B Tile -> (V2 Int, Tile) -> [(V2 Int, Tile)]
neighbors grid (pos, _) = [ (pos', tile') 
                          | pos' <- adjacent pos
                          , let tile' = grid ! toIx2 pos'
                          , tile' /= Wall 
                          ]


neighbors1 :: V2 Int -> Matrix B Tile -> (V2 Int, Tile) -> [(V2 Int, Tile)]
neighbors1 start grid (pos, tile) | pos /= start && isInterestingTile tile = []
                                  | otherwise = [ (pos', tile') 
                                                | pos' <- adjacent pos
                                                , let tile' = grid ! toIx2 pos'
                                                , tile' /= Wall 
                                                ]

neighbors2 :: Graph -> (Tile, Keys) -> [((Tile, Keys), Int)]
neighbors2 graph (v, keys) = do
    (u, w) <- graph Map.! v
    case u of
        Key k -> pure ((u, k .|. keys), w)
        Door d | d .&. keys == 0 -> []
        _ -> pure ((u, keys), w)

part1 :: Matrix B Tile -> Maybe Int
part1 grid = do
    let allKeys = (1 `shiftL` length [() | Key _ <- A.toList grid]) - 1
    start <- listToMaybe [ V2 i j 
                         | (i, j, Start) <- flattenWithIndex (A.toLists2 grid)
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
    start <- listToMaybe [ V2 i j 
                         | (i, j, Start) <- flattenWithIndex (A.toLists2 grid)
                         ]
    let starts = surrounding start \\ adjacent start
    let newWalls = start : adjacent start
    let grid' = grid // ([(toIx2 p, Start) | p <- starts] ++ [(toIx2 p, Wall) | p <- newWalls])
    let graphs = map (mkGraph grid') starts
    let isGoal (SearchState _ keys) = keys == allKeys
    dijkstra (neighbors3 graphs) isGoal (SearchState (V.replicate 4 Start) 0)

solve :: Text -> IO ()
solve = aoc parser part1 part2
