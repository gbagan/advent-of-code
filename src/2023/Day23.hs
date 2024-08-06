-- https://adventofcode.com/2023/day/23
module Day23 (solve) where
import           AOC.Prelude hiding (pred, fromList)
import           AOC (aoc)
import           Data.Massiv.Array (Array, Matrix, (!), (!?), fromList, fromLists', imapM, size
                                    , B, Comp(Seq), Sz(..), Ix1, Ix2(..))
import           AOC.Parser (Parser, sepEndBy1, eol, some, choice)
import           GHC.Bits
import           AOC.List (maximumDef)
import           AOC.V2 (V2(..), adjacent, up, down, left, right, toIx2)
-- import           AOC.Graph (longestPath)

data Tile = Path | Forest | North | South | West | East deriving (Eq)
type Grid = Matrix B Tile

parser :: Parser Grid
parser = fromLists' Seq <$> some tile `sepEndBy1` eol where
    tile = choice [Path <$ ".", Forest <$ "#", North <$ "^", South <$ "v", West <$ "<", East <$ ">"]

neighbors1 :: Grid -> V2 Int -> [V2 Int]
neighbors1 grid p = case grid ! toIx2 p of
    Path -> [ p'
            | p' <- adjacent p
            , let tile = grid !? toIx2 p'
            , isJust tile && tile /= Just Forest
            ]
    North -> [up p]
    South -> [down p]
    West -> [left p]
    East -> [right p]
    _ -> []

neighbors2 :: Grid -> V2 Int -> [V2 Int]
neighbors2 grid p = 
    if (grid ! toIx2 p) == Forest
        then []
        else [ p' 
             | p' <- adjacent p
             , let tile = grid !? toIx2 p'
             , isJust tile && tile /= Just Forest
             ]

compressGrid :: (Grid -> V2 Int -> [V2 Int]) -> Grid -> Array B Ix1 [(Int, Int)]
compressGrid neighbors grid = arr
    where
    followPath pos pred len =
        case neighbors grid pos of
            [next]         | next /= pred  -> followPath next pos $! len+1
            [_]            | pos /= dest   -> Nothing
            [next1, next2] | next1 == pred -> followPath next2 pos $! len+1
                           | otherwise     -> followPath next1 pos $! len+1
            _ -> Just (pos, len)

    Sz2 h w = size grid
    start = V2 0 1
    dest = V2 (h-1) (w-2)
    (grid', (_, junctions)) = flip runState (0, []) $ imapM @B (\ix@(Ix2 y x) _ -> do
        (n, junctions') <- get
        let pos = V2 y x
            nbors = neighbors grid pos
        if pos == start || pos == dest || length nbors > 2 then do
            put (n+1, ix : junctions')
            pure (n, catMaybes [followPath next pos 1 | next <- nbors])
        else
            pure (0, [])
        ) grid
    normNbors ix =
        let (_, nbors) = grid' ! ix in
        map (\(pos, len) -> (fst (grid' ! toIx2 pos), len)) nbors
    arr = fromList Seq . reverse $ map normNbors junctions

{-
previousBorder :: Array B Ix1 [(Int, Int)] -> Array  B Ix1 [Maybe Int]
previousBorder arr = 0 where
    Sz n = size arr
    start = 0
    [(start', _)] = arr ! start
    end = n-1
    [(end', _)] = arr ! end    
    [next1, next2] = [nbor | (nbor, _) <- arr ! start', nbor /= start]
    go prev current | current = end' = []
                    | otherwise =
-}

longestPath :: (Int -> [(Int, Int)]) -> Int -> Int -> Int
longestPath neighbors start dest = go (0::Int) start 0 where
    go !visited !pos !len
        | pos == dest = len
        | otherwise =
            let !spos = 1 `unsafeShiftL` pos in
            maximumDef 0 [ go (spos .|. visited) next $! len+len'
                         | (next, len') <- neighbors pos
                         , spos .&. visited == 0
                         ]

solveFor :: (Grid -> V2 Int -> [V2 Int]) -> Grid -> Int
solveFor neighbors grid = longestPath neighbors' start dest where
    compressed = compressGrid neighbors grid
    neighbors' p = compressed ! p
    Sz n = size compressed
    start = 0
    dest = n-1

solve :: Text -> IO ()
solve = aoc parser (solveFor neighbors1) (solveFor neighbors2)