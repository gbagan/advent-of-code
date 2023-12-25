module AOC.Graph.MaximumMatching where
import           AOC.Prelude hiding (head, tail, init)
import           Control.Monad.ST (ST, runST)
import qualified Data.HashSet as Set
import qualified Data.HashMap.Strict as Map
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import           AOC.List (grouped2)
import           AOC.Monad (findM)
import           AOC.Graph.Search (dfsM')

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

findPath :: MV.MVector s (Maybe Int) -> Int -> ST s [Int]
findPath parents v = do
    mParent <- MV.read parents v
    case mParent of
        Nothing -> pure [v]
        Just parent -> do
            xs <- findPath parents parent
            pure $! v : xs 

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