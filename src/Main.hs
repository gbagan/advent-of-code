module Main where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Day01 (solve)
import qualified Day02 (solve)
import qualified Day03 (solve)
import qualified Day04 (solve)
import qualified Day05 (solve)
import qualified Day06 (solve)
import qualified Day07 (solve)
import qualified Day08 (solve)
import qualified Day09 (solve)
import qualified Day10 (solve)
import qualified Day11 (solve)
import qualified Day12 (solve)
import qualified Day13 (solve)
import qualified Day14 (solve)
import System.Environment (getArgs)

solutions :: Map String (String -> Maybe (Int, Int))
solutions = Map.fromList
            [   ("01", Day01.solve)
            ,   ("02", Day02.solve)
            ,   ("03", Day03.solve)
            ,   ("04", Day04.solve)
            ,   ("05", Day05.solve)
            ,   ("06", Day06.solve)
            ,   ("07", Day07.solve)
            ,   ("08", Day08.solve)
            ,   ("09", Day09.solve)
            ,   ("10", Day10.solve)
            ,   ("11", Day11.solve)
            ,   ("12", Day12.solve)
            ,   ("13", Day13.solve)
            ,   ("14", Day14.solve)
            ]

solveProblem :: String -> IO ()
solveProblem name = case Map.lookup name solutions of
    Just f -> do
        putStrLn $ "Solve day " ++ name
        s <- readFile ("./data/data" ++ name)
        case f s of
            Nothing -> putStrLn "Parsing problem or no solution found"
            Just (sol1, sol2) -> do
                putStrLn $ "  part 1: " ++ show sol1
                putStrLn $ "  part 2: " ++ show sol2
    Nothing -> putStrLn $ "Day not implemented: " ++ name

main :: IO ()
main = do
    args <- getArgs
    mapM_ solveProblem if null args then Map.keys solutions else args