-- https://adventofcode.com/2016/day/17
module Day17 (solve) where
import           AOC.Prelude hiding (Left, Right, Down, head, last)
import           Data.List (maximum)
import           AOC (aoc)
import           AOC.Parser (takeRest) 
import qualified Data.Text.Encoding as TSE
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Base16 as B16 (encode)
import           Crypto.Hash.MD5 (hash)
import           AOC.V2 (V2(..), origin, up, down, left, right) 
import           AOC.Area (Area(..))
import qualified AOC.Area as Area
import           AOC.List (minimumOn)

type Node = (String, V2 Int)

-- faster than BFS since we don't check if a node has already been visited
allPaths :: (a -> [a]) -> (a -> Bool) -> a -> [a]
allPaths nbors isGoal node
    | isGoal node = [node]
    | otherwise = concatMap (allPaths nbors isGoal) (nbors node)

gridArea :: Area Int
gridArea = Area 0 0 3 3

nextPositions :: ByteString -> Node -> [Node]
nextPositions seed (dirs, pos)
    | pos == V2 3 3 = []
    | otherwise =
        [ (dirs++[dir], nextPos)
        | ((dir, nextPos), c) <- zip posDirs hashed
        , c >= 'b'
        , nextPos `Area.elem` gridArea
        ]
    where
    hashed = C8.unpack . B16.encode . hash $ seed <> C8.pack dirs
    posDirs = [('U', up pos), ('D', down pos), ('L', left pos), ('R', right pos)]

getPaths :: ByteString -> [Node]
getPaths seed = allPaths (nextPositions seed) ((== V2 3 3) . snd) ([], origin)

part1 :: ByteString -> String
part1 = fst . minimumOn (length . fst) . getPaths

part2 :: ByteString -> Int
part2 = maximum . map (length . fst) . getPaths

solve :: Text -> IO ()
solve = aoc (TSE.encodeUtf8 <$> takeRest) part1 part2