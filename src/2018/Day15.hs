-- https://adventofcode.com/2018/day/15
module Day15 (solve) where
import           AOC.Prelude hiding (get, head)
import qualified Data.HashMap.Strict as Map
import           Data.Massiv.Array (Matrix, B, Comp(Seq), (!), fromLists')
import           AOC (aoc')
import           AOC.List (findJust, flattenWithIndex', groupOn, headMaybe, tailMaybe, minimumOn)
import           AOC.Parser (Parser, choice, sepEndBy1, some, eol)
import           AOC.V2 (V2(..), up, down, left, right, toIx2)
import           AOC.Graph (bfs, shortestPath)

data Race = Elf | Goblin deriving (Eq, Ord, Show)
data Tile = Empty | Wall | Character !Int !Race !Int deriving (Eq, Show)
data Grid =  Grid !(Matrix B Tile) !(HashMap (V2 Int) Tile)

hasId :: Tile -> Int -> Bool
hasId (Character id' _ _) id_ | id_ == id' = True
hasId _ _ = False

parser :: Parser [[Tile]]
parser = some tile `sepEndBy1` eol where
    tile = choice
            [ "#" $> Wall
            , "." $> Empty
            , "G" $> Character 0 Goblin 200
            , "E" $> Character 0 Elf 200
            ]

mkGrid :: [[Tile]] -> Grid
mkGrid tiles = Grid mat map_ where
    mat = fromLists' Seq $ map (map removeChar) tiles
    map_ = Map.fromList [ (pos, Character i race 200)
                        | (i, (pos, Character _ race _)) <- zip [0..] (flattenWithIndex' tiles)
                        ]                           
    removeChar Wall = Wall
    removeChar _ = Empty

get :: V2 Int -> Grid -> Tile
get pos (Grid mat map_) =
    case map_ Map.!? pos of
        Just tile -> tile
        Nothing -> mat ! toIx2 pos

set :: V2 Int -> Tile -> Grid -> Grid
set pos tile (Grid mat map_) = Grid mat (Map.insert pos tile map_)

remove :: V2 Int -> Grid -> Grid
remove pos (Grid mat map_) = Grid mat (Map.delete pos map_)

adjacent :: V2 Int -> [V2 Int]
adjacent p = [up p, left p, right p, down p]

neighbors :: V2 Int -> Grid -> V2 Int -> [V2 Int]
neighbors start grid v
    | start /= v && get v grid /= Empty = []
    | otherwise                         = adjacent v

moveAndAttack :: Int -> Grid -> (V2 Int, Int, Race, Int) -> Grid
moveAndAttack elfAttack grid (pos, id_, race, life) = fromMaybe grid do
    guard $ get pos grid `hasId` id_ -- check if the character who does action is still alive
    nearestAdversories <- headMaybe $ groupOn fst         
            [ (d, (pos', id', race', life'))
            | (d, pos') <- bfs (neighbors pos grid) pos
            , Character id' race' life' <- [get pos' grid]
            , race' /= race
            ]
    let (dist, (pos', _, _, _)) = minimumOn (\(_, (p, _, _, _)) -> p) nearestAdversories
    if dist <= 1
        then pure $ attack elfAttack grid (pos, race)
        else do
            nextPos <- headMaybe =<< tailMaybe =<< shortestPath (neighbors pos grid) pos pos'
            let grid' = set nextPos (Character id_ race life) $ remove pos grid
            pure $ attack elfAttack grid' (nextPos, race)

attack :: Int -> Grid -> (V2 Int, Race) -> Grid
attack elfAttack grid (pos, race) =
    let nearestAdversories = [ (pos', id', race', life')
                             | pos' <-adjacent pos
                             , Character id' race' life' <- [get pos' grid]
                             , race' /= race
                             ]
    in case nearestAdversories of
        [] -> grid
        _ ->
            let (pos', id', race', life') = minimumOn (\(p, _, _, l) -> (l, p)) nearestAdversories
                attack' = if race == Elf then elfAttack else 3
            in if life' <= attack'
                then remove pos' grid
                else set pos' (Character id' race' (life'-attack')) grid

step :: Int -> Grid -> Grid
step elfAttack grid@(Grid _ map_) = foldl' (moveAndAttack elfAttack) grid characterPositions where
    characterPositions = sortOn (\(x, _, _, _) -> x) [ (p, id_, race, life) 
                                                     | (p, Character id_ race life) <- Map.toList map_
                                                     ]

isFinished :: Grid -> Bool
isFinished (Grid _ map_) = length (ordNub [race | Character _ race _ <- Map.elems map_]) == 1

score :: Int -> Grid -> Int
score n (Grid _ map_) = (n-1) * sum [l | Character _ _ l <- Map.elems map_]

nbAliveElves :: Grid -> Int
nbAliveElves (Grid _ map_) = length [() | Character _ Elf _ <- Map.elems map_]

noElfDies :: Int -> Grid -> Maybe Int
noElfDies elfAttack initGrid = go 0 initGrid where
    go i grid =
        let grid' = step elfAttack grid
            alive = nbAliveElves grid'
        in if | alive /= initAlive -> Nothing
              | isFinished grid    -> Just $ score i grid'
              | otherwise          -> go (i+1) grid'
    initAlive = nbAliveElves initGrid

part1 :: Grid -> Int
part1 grid = score n grid' where
    (n, grid') = findJust (isFinished . snd) $ iterate' (\(i, g) -> (i+1, step 3 g)) (0, grid)  

part2 :: Grid -> Maybe Int
part2 grid = headMaybe $ catMaybes [ noElfDies elfAttack grid
                                   | elfAttack <- [4..]
                                   ]


solve :: Text -> IO ()
solve = aoc' parser (Just . mkGrid) part1 part2