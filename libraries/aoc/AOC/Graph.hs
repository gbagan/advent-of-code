module AOC.Graph where
import           AOC.Prelude hiding (init)
-- import           Control.Monad.ST (ST, runST)
import           Data.Sequence (Seq(..), (><))
import qualified Data.Sequence as Seq
import qualified Data.HashSet as Set
import qualified Data.HashMap.Strict as Map
import qualified Data.HashPSQ as Q
-- import qualified Data.Vector as V
-- import qualified Data.Vector.Mutable as MV
-- import           AOC.Monad (findM)
import           AOC.List (groupOn, minimumOn, maximumDef)

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
dijkstra' nbors targetFunc sources = go Set.empty initialQueue where
    initialQueue = Q.fromList [ (s,0,()) | s <- sources]
    go !visited !queue = case Q.minView queue of
        Nothing -> Nothing
        Just (v, cost, _, queue')
            | targetFunc v -> Just $! cost
            | otherwise -> go
                            (Set.insert v visited)
                            (foldl' insert queue'
                                [ (v', cost+w') | (v', w') <- nbors v, not (v' `Set.member` visited)]
                            )
    insert queue (u, w) = case Q.lookup u queue of
        Just (w', _) | w' < w -> queue
        _ -> Q.insert u w () queue

{-# INLINE dijkstra' #-}

longestPath :: Hashable a => (a -> [(a, Int)]) -> a -> a -> Int
longestPath neighbors start dest = go Set.empty 0 start where
    go visited len pos
        | pos == dest = len
        | otherwise = maximumDef 0 [ go (Set.insert pos visited) (len+len') next
                                   | (next, len') <- neighbors pos
                                   , not $ next `Set.member` visited
                                   ]

{-# INLINE longestPath #-}

perfectMatchings :: (Eq a, Hashable b) => [(a, [b])] ->  [[(a, b)]]
perfectMatchings = go . map (second Set.fromList) where
    go [] = [[]]
    go g = do
        let (v, nbors) = minimumOn (Set.size . snd) g
        u <- Set.toList nbors
        ((v, u) :) <$> go [ (w, Set.delete u nbors')  | (w, nbors') <- g, v /= w]

{-# INLINE perfectMatchings #-}

connectedComponents :: Hashable a => HashMap a (HashSet a) -> [[a]]
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

-- _minimumCutPhase :: Hashable a => HashMap a (HashSet a) -> [(a, a)]


{-

type IntGraph = (Int, Vector [Int])
type BipartiteGraph a b = HashMap a [b]
type Matching a b = HashMap a b

_greedyMatching :: IntGraph -> Matching Int Int
_greedyMatching (m, graph) =
    Map.fromList . catMaybes $ runST do
        matched <- MV.replicate m False
        zipWithM (go matched) [0..] (V.toList graph)
    where
    go matched i nbors = do
        mx <- findM (fmap not . MV.read matched) nbors
        case mx of
            Nothing -> pure Nothing
            Just x -> do
                MV.write matched x True
                pure $ Just (i, x)

_findAugmentingPath :: IntGraph -> Matching Int Int -> Maybe (Matching Int Int)
_findAugmentingPath (m, graph) matching = runST do
    matched <- MV.replicate (n+m) Nothing
    parents <- MV.replicate (n+m) Nothing
    forM_ (Map.toList matching) \(u, v) -> do
        MV.write matched u (Just (n+v))
        MV.write matched (n+v) (Just u)
    unexplored <- filterM (fmap isNothing . MV.read matched) [0..n-1]
    dfsM' (nborFunc parents matched) unexplored
    mSink <- [n..n+m-1] & findM \i -> do
        match <- MV.read matched i 
        parent <- MV.read parents i
        pure (isNothing match && isJust parent)
    case mSink of
        Nothing -> pure Nothing
        Just sink -> do
            path <- findPath parents sink
            let pairs = grouped2 (reverse path)
            pure . Just $ foldl' (\m' (x, y) -> Map.insert x (y-n) m') matching pairs

    where
    n = V.length graph
    nborFunc :: MV.MVector s (Maybe Int) -> MV.MVector s (Maybe Int) -> Int -> ST s [Int]
    nborFunc parents matched i =
        if i < n then do
            let nbors = map (+n) $ graph V.! i
            forM_ nbors \nbor -> do
                MV.modify parents (<|> Just i) nbor
            pure nbors
        else do
            match <- MV.read matched i
            case match of
                Nothing -> pure [] 
                Just u -> do
                    MV.modify parents (<|> Just i) u
                    pure [u]
    findPath :: MV.MVector s (Maybe Int) -> Int -> ST s [Int]
    findPath parents v = do
        mParent <- MV.read parents v
        case mParent of
            Nothing -> pure [v]
            Just parent -> do
                xs <- findPath parents parent
                pure $! v : xs 

_maximumMatching :: IntGraph -> Matching Int Int
_maximumMatching g = go (_greedyMatching g) where
    go m = maybe m go (_findAugmentingPath g m)

maximumMatching :: (Hashable a, Hashable b) => BipartiteGraph a b -> Matching a b
maximumMatching g = m' where
    verticesInA = Map.keys g
    verticesInB = Set.toList $ Set.unions [Set.fromList nbor | nbor <- Map.elems g]
    vA = V.fromList verticesInA
    vB = V.fromList verticesInB
    dictB = Map.fromList $ zip verticesInB [0..]
    g' = (V.length vB, V.fromList . map (map (dictB Map.!)) $ Map.elems g)
    m = _maximumMatching g'
    m' = Map.fromList . map go $ Map.toList m
    go (i, j) = (vA V.! i, vB V.! j)

{-# INLINE maximumMatching #-}
-}