module AOC.Graph.Search where
import           AOC.Prelude hiding (head, tail, init)
import           Data.Sequence (Seq(..), (><))
import qualified Data.Sequence as Seq
import qualified Data.HashSet as Set
import qualified Data.HashMap.Strict as Map
import qualified Data.HashPSQ as Q
import           AOC.List (groupOn, maximumDef)
import           AOC.Graph.Base (Graph)

bfsOn :: Hashable r => (a -> r) -> (a -> [a]) -> a -> [(Int, a)]
bfsOn rep nborFunc start = go Set.empty (Seq.singleton (0, start)) where
    go _ Empty = []
    go visited ((d, v) :<| queue)
        | r `Set.member` visited = go visited queue
        | otherwise = (d, v) : go
                        (Set.insert r visited)
                        (queue >< Seq.fromList [ (d+1, u) | u <- nborFunc v])
        where r = rep v
{-# INLINE bfsOn #-}

bfs :: Hashable a => (a -> [a]) -> a -> [(Int, a)]
bfs = bfsOn id
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

astar :: (Ord a, Hashable a) => a -> (a -> Bool) -> (a -> [(a, Int)]) -> (a -> Int) -> Maybe Int
astar startNode isGoal neighbors heuristic =
    astarAux
        (Q.singleton startNode (heuristic startNode) 0)
        Set.empty
        (Map.singleton startNode 0)
    where
    astarAux pq seen gscore = case Q.minView pq of
        Nothing -> Nothing
        Just (node, _, gcost, pq')
            | isGoal node      -> Just gcost
            | Set.member node seen -> astarAux pq' seen gscore
            | otherwise            -> astarAux pq'' seen' gscore'
            where
            seen'      = Set.insert node seen
            successors = 
                [ (u, g', heuristic u) 
                | (u, g) <- neighbors node
                , let g' = gcost + g
                , not (u `Map.member` gscore) || g' < (gscore Map.! u)
                ]
            pq''    = foldl' (\q (s, g, h) -> Q.insert s (g + h) g q) pq' successors
            gscore' = foldl' (\m (s, g, _) -> Map.insert s g m) gscore successors

{-# INLINE astar #-}

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
longestPath neighbors start dest = go Set.empty start 0 where
    go visited pos len
        | pos == dest = len
        | otherwise = maximumDef 0 [ go (Set.insert pos visited) next $! len+len'
                                   | (next, len') <- neighbors pos
                                   , not $ next `Set.member` visited
                                   ]

{-# INLINE longestPath #-}