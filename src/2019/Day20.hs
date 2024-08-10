-- https://adventofcode.com/2019/day/20
module Day20 (solve) where
import           AOC.Prelude
import           Control.Monad.ST (ST, runST)
import qualified Data.HashMap.Strict as Map
import           Data.Massiv.Array (Matrix, (!), (!?), iunfoldlPrimM, fromLists', size, B, U, Comp(Seq), Ix2(..), Sz(..))
import           AOC (aoc')
import           AOC.List (headMaybe)
import           AOC.Parser (Parser, choice, char, upperChar, sepEndBy1, some, eol)
import           AOC.V2 (V2(..), adjacent, origin, east, south, toIx2)
import           AOC.Graph (bfs, dijkstra)

data Tile = Empty | Open | Wall | Warp (String, Bool) deriving (Eq)
type Warp = (String, Bool)
type Warps = HashMap Warp (V2 Int)
type WarpGraph = HashMap Warp [(Warp, Int)]
data Node = Node !Int !String !Bool deriving (Eq, Ord, Generic)
instance Hashable Node

parser :: Parser (Matrix U Char)
parser = fromLists' Seq <$> some tile `sepEndBy1` eol where
    tile = choice [upperChar, char '#', char ' ', char '.']

processInput :: Matrix U Char -> WarpGraph
processInput grid = mkWarpGraph grid' warps where
    (warps, grid') = runST $ iunfoldlPrimM (size grid) go Map.empty

    go :: Warps -> Ix2 -> ST s (Warps, Tile)
    go ws idx = case grid ! idx of
        '.' -> pure (ws, Open)
        '#' -> pure (ws, Wall)
        ' ' -> pure (ws, Empty)
        c | Just (pos, warp) <- processLetter idx c ->
            let warp' = (warp, isOuter pos)
            in pure (Map.insert warp' pos ws, Warp warp')
        _ -> pure (ws, Empty)

    isOuter :: V2 Int -> Bool
    isOuter (V2 y x) = x <= 1 || y <= 1 || x >= w - 2 || y >= h - 2 where
        Sz2 h w = size grid

    processLetter :: Ix2 -> Char -> Maybe (V2 Int, String)
    processLetter (Ix2 y x) c =
        headMaybe $ mapMaybe (\dir ->
            let pos = V2 y x in
            case (grid !? toIx2 (pos - dir), grid !? toIx2 (pos + dir)) of
                (Just c', Just '.') -> Just (pos, if dir == east || dir == south then [c', c] else [c, c'])
                _ -> Nothing
        )
        (adjacent origin)

mkWarpGraph :: Matrix B Tile -> Warps -> WarpGraph
mkWarpGraph grid warps = Map.fromList $ map go (Map.toList warps) where
    go (warp, pos) = (warp, do
        (dist, pos') <- drop 1 $ bfs neighbors pos
        case grid ! toIx2 pos' of
            Warp warp' -> pure (warp', dist-1)
            _ -> []
        )
    neighbors :: V2 Int -> [V2 Int]
    neighbors pos = [ pos'
                    | pos' <- adjacent pos
                    , let tile = grid !? toIx2 pos'
                    , isJust tile && tile /= Just Wall && tile /= Just Empty
                    ]

solveFor :: (Bool -> Int -> Int) -> WarpGraph -> Maybe Int
solveFor f graph = subtract 1 <$> dijkstra neighbors (== Node 0 "ZZ" True) (Node 0 "AA" False) where
    neighbors :: Node -> [(Node, Int)]
    neighbors (Node depth warp outer) 
        | warp == "ZZ" && depth > 0 = []
        | otherwise = [ (Node depth' warp' outer', dist)
                      | ((warp', outer'), dist) <- Map.findWithDefault [] (warp, not outer) graph
                      , let depth' = if depth == 0 && warp' == "ZZ" then 0 else  f outer' depth
                      , (depth' >= 0 && warp' /= "ZZ") || (warp' == "ZZ" && depth == 0)
                      ]

part1 :: WarpGraph -> Maybe Int
part1 = solveFor $ \_ _ -> 0

part2 :: WarpGraph -> Maybe Int
part2 = solveFor $ bool (+1) (subtract 1)

solve :: Text -> IO ()
solve = aoc' parser (Just . processInput) part1 part2