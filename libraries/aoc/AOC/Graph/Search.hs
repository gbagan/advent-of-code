module AOC.Graph.Search where
import           AOC.Prelude hiding (head, tail, init)
import           Data.Sequence (Seq(..), (><))
import qualified Data.Sequence as Seq
import qualified Data.HashSet as Set
import qualified Data.HashMap.Strict as Map
import qualified Data.HashPSQ as Q
import           AOC.List (groupOn, maximumDef)
import           AOC.Graph.Base (Graph)

bfs :: Hashable a => (a -> [a]) -> a -> [(Int, a)]
bfs nborFunc start = go Set.empty (Seq.singleton (0, start)) where
    go _ Empty = []
    go visited ((d, v) :<| queue)
        | v `Set.member` visited = go visited queue
        | otherwise = (d, v) : go
                        (Set.insert v visited)
                        (queue >< Seq.fromList [ (d+1, u) | u <- nborFunc v])

{-# INLINE bfs #-}

distance :: Hashable a => (a -> [a]) -> (a -> Bool) -> a -> Maybe Int
distance nborFunc destFunc start =
    fst <$> find (destFunc . snd) (bfs nborFunc start)

{-# INLINE distance #-}

reachableFrom :: Hashable a => (a -> [a]) -> a -> HashSet a
reachableFrom nborFunc = reachableFrom' \v _ -> nborFunc v

{-# INLINE reachableFrom #-}

reachableFrom' :: Hashable a => (a -> HashSet a -> [a]) -> a -> HashSet a
reachableFrom' nborFunc start = go Set.empty [start] where
    go visited [] = visited
    go visited (v:queue)
        | v `Set.member` visited = go visited queue
        | otherwise =
            let visited' = Set.insert v visited
                nbors = nborFunc v visited'
            in go visited' (nbors ++ queue)

{-# INLINE reachableFrom' #-}

dfsM :: (Hashable a, Monad m) => (a -> m [a]) -> a -> m ()
dfsM nborFunc start = dfsM' nborFunc [start]

{-# INLINE dfsM #-}

dfsM' :: (Hashable a, Monad m) => (a -> m [a]) -> [a] -> m ()
dfsM' nborFunc = go Set.empty where
    go _ [] = pure ()
    go visited (v:queue)
        | v `Set.member` visited = go visited queue
        | otherwise = do
            nbors <- nborFunc v
            go (Set.insert v visited) (nbors ++ queue)

{-# INLINE dfsM' #-}

dijkstra :: (Hashable v, Ord v, Real w) => (v -> [(v, w)]) -> (v -> Bool) -> v -> Maybe w
dijkstra nborFunc targetFunc source = dijkstra' nborFunc targetFunc [source]

{-# INLINE dijkstra #-}

dijkstra' :: (Hashable v, Ord v, Real w) => (v -> [(v, w)]) -> (v -> Bool) -> [v] -> Maybe w
dijkstra' nbors targetFunc sources = aux Set.empty initialQueue where
    initialQueue = Q.fromList [ (s,0,()) | s <- sources]
    aux visited queue = case Q.minView queue of
        Nothing -> Nothing
        Just (v, cost, _, queue')
            | targetFunc v -> Just $! cost
            | otherwise -> aux
                            (Set.insert v visited)
                            (foldl' insert queue'
                                [ (v', cost+w') | (v', w') <- nbors v, not (v' `Set.member` visited)]
                            )
    insert queue (u, w) = case Q.lookup u queue of
        Just (w', _) | w' < w -> queue
        _ -> Q.insert u w () queue

{-# INLINE dijkstra' #-}

astar :: (Eq a, Ord a, Hashable a) => a -> (a -> Bool) -> (a -> [(a, Int)]) -> (a -> Int) -> Maybe (Int, [a])
astar startNode isGoalNode nextNodeFn heuristic =
        astarAux
            (Q.singleton startNode (heuristic startNode) 0)
            Set.empty
            (Map.singleton startNode 0)
            Map.empty
    where
    astarAux :: Q.HashPSQ a Int Int -> HashSet a -> HashMap a Int -> HashMap a a -> Maybe (Int, [a])
    astarAux pq seen gscore tracks = case Q.minView pq of
        Nothing -> Nothing
        Just (node, _, gcost, pq')
            | isGoalNode node      -> Just (gcost, reverse (findPath tracks node))
            | Set.member node seen -> astarAux pq' seen gscore tracks
            | otherwise            -> astarAux pq'' seen' gscore' tracks'
            where
            seen' :: HashSet a
            seen'       = Set.insert node seen
            successors :: [(a, Int, Int)]
            successors  = 
                filter (\(s, g, _) -> not (Set.member s seen') &&
                    (not (s `Map.member` gscore) || g < (gscore Map.! s)))
                $ successorsAndCosts node gcost
            pq''    = foldl' (\q (s, g, h) -> Q.insert s (g + h) g q) pq' successors
            gscore' = foldl' (\m (s, g, _) -> Map.insert s g m) gscore successors
            tracks' = foldl' (\m (s, _, _) -> Map.insert s node m) tracks successors
    successorsAndCosts :: a -> Int -> [(a, Int, Int)]
    successorsAndCosts v gcost = [(u, gcost + g, heuristic u) | (u, g) <- nextNodeFn v]
    findPath tracks node =
        case tracks Map.!? node of
            Nothing -> [node]
            Just s ->  node : findPath tracks s

connectedComponents :: Hashable a => Graph a -> [[a]]
connectedComponents g = map (map fst) . groupOn snd . sortOn snd $ Map.toList a
    where
    a = fst $ foldl' go (Map.empty, 0::Int) (Map.toList g)
    go (mapped, idx) (v, _)
        | v `Map.member` mapped = (mapped, idx)
        | otherwise =
            ( Map.union
                mapped
                (Map.fromList . map (,idx)
                    . Set.toList $ reachableFrom (Set.toList . (g Map.!)) v
                )
            , idx+1
            )

{-# INLINE connectedComponents #-}

longestPath :: Hashable a => (a -> [(a, Int)]) -> a -> a -> Int
longestPath neighbors start dest = go Set.empty 0 start where
    go visited len pos
        | pos == dest = len
        | otherwise = maximumDef 0 [ go (Set.insert pos visited) (len+len') next
                                   | (next, len') <- neighbors pos
                                   , not $ next `Set.member` visited
                                   ]

{-# INLINE longestPath #-}