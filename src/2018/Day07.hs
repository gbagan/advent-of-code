-- https://adventofcode.com/2018/day/7
module Day07 (solve) where
import           AOC.Prelude
import           AOC (aoc)
import           AOC.Parser (Parser, sepEndBy1, eol, upperChar, scanf)
import qualified Data.Set as Set
import qualified AOC.HashMap as Map
import           AOC.Graph (lexicographicTopologicalOrdering)
import           Data.List (minimum)


parser :: Parser [(Char, Char)]
parser = [scanf|Step {upperChar} must be finished before step {upperChar} can begin.|] `sepEndBy1` eol

part1 :: [(Char, Char)] -> String
part1 = lexicographicTopologicalOrdering

duration :: Char -> Int
duration c = ord c - ord 'A'  + 61

part2 :: [(Char, Char)] -> Int
part2 edges = go 0 initialQueue [] incomingDegree where
    outgoingTasks :: HashMap Char [Char]
    outgoingTasks = foldl' (\deg (u, v) -> Map.insertWith (++) u [v] deg) Map.empty edges
    incomingDegree :: HashMap Char Int
    incomingDegree = foldl' (\deg (_, v) -> Map.insertWith (+) v (1::Int) deg) Map.empty edges
    initialQueue :: Set Char
    initialQueue = Set.fromList [u | (u, _) <- edges, u `Map.notMember` incomingDegree]

    go :: Int -> Set Char -> [(Char, Int)] -> HashMap Char Int -> Int
    go time queue workers degrees | length workers < 5, Just (task, queue') <- Set.minView queue =
                                        go time queue' ((task, duration task) : workers) degrees
                                  | null workers = time
                                  | otherwise =
                                        let minWorker = minimum (map snd workers)
                                            workers' = map (second (subtract minWorker)) workers
                                            (done, workers'') = partition ((==0) . snd) workers'
                                            (degrees', queue') = foldl' updateDegreesAndQueue (degrees, queue) (map fst done)
                                        in
                                            go (time+minWorker) queue' workers'' degrees'


    updateDegreesAndQueue :: (HashMap Char Int, Set Char) -> Char -> (HashMap Char Int, Set Char)
    updateDegreesAndQueue degrees task = foldl' go' degrees nexts where
        nexts = Map.lookupDefault [] task outgoingTasks
        go' (degs, queue) v =
            let deg = degs Map.! v - 1 in
            if deg == 0
                then (degs, Set.insert v queue)
                else (Map.insert v deg degs, queue)


solve :: Text -> IO ()
solve = aoc parser part1 part2