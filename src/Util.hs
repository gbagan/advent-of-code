module Util where
import Data.List (sort, genericLength)
import Data.Map (Map)
import Data.Void (Void)
import           Text.Megaparsec (Parsec, parse, errorBundlePretty)
import qualified Data.Map as Map

type Parser = Parsec Void String
type Point = (Int, Int)

aocTemplate :: Parser a -> (a -> Maybe Int) -> (a -> Maybe Int) -> String -> IO ()
aocTemplate parser part1 part2 s = do
    case parse parser "" s of
        Left err -> putStr (errorBundlePretty err)
        Right input -> do
            case part1 input of
                Nothing -> putStrLn "  part 1: no solution found"
                Just n ->  putStrLn $ "  part 1: " ++ show n
            case part2 input of
                Nothing -> putStrLn "  part 2: no solution found"
                Just n ->  putStrLn $ "  part 2: " ++ show n

adjacentPoints :: Point -> [Point]
adjacentPoints (x, y) = [(x-1, y), (x+1, y), (x, y-1), (x, y+1)]

kingAdjacentPoints :: Point -> [Point]
kingAdjacentPoints (x, y) = adjacentPoints (x, y) ++ [(x-1, y-1), (x+1, y-1), (x-1, y+1), (x+1, y+1)]

count :: (a -> Bool) -> [a] -> Int
count f = length . filter f

freqs :: Ord a => [a] -> Map a Int
freqs = Map.fromListWith (+) . map (,1)

average :: [Int] -> Double
average xs = realToFrac (sum xs) / genericLength xs

majority :: (a -> Bool) -> [a] -> Bool
majority f l = 2* count f l >= length l

median :: Ord a => [a] -> a
median l = sort l !! (length l `div` 2)

listTo2dMap :: [[a]] -> Map Point a
listTo2dMap l = 
    Map.fromList
        [((i, j), v) 
        | (j, row) <- zip [0..] l
        , (i, v) <- zip [0..] row
        ]