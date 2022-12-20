module Main where
import           RIO
import qualified RIO.Text as Text
import qualified RIO.Map as Map
import qualified AOC2022.Day01 (solve)
import qualified AOC2022.Day02 (solve)
import qualified AOC2022.Day03 (solve)
import qualified AOC2022.Day04 (solve)
import qualified AOC2022.Day05 (solve)
import qualified AOC2022.Day06 (solve)
import qualified AOC2022.Day07 (solve)
import qualified AOC2022.Day08 (solve)
import qualified AOC2022.Day09 (solve)
import qualified AOC2022.Day10 (solve)
import qualified AOC2022.Day11 (solve)
import qualified AOC2022.Day12 (solve)
import qualified AOC2022.Day13 (solve)
import qualified AOC2022.Day14 (solve)
import qualified AOC2022.Day15 (solve)
import qualified AOC2022.Day16 (solve)
import qualified AOC2022.Day17 (solve)
import qualified AOC2022.Day18 (solve)
import qualified AOC2022.Day19 (solve)
import qualified AOC2022.Day20 (solve)
import System.Environment (getArgs)
import Data.Text.IO (putStrLn)

solutions :: Map Text (Text -> RIO SimpleApp ())
solutions = Map.fromList
            [   ("01", AOC2022.Day01.solve)
            ,   ("02", AOC2022.Day02.solve)
            ,   ("03", AOC2022.Day03.solve)
            ,   ("04", AOC2022.Day04.solve)
            ,   ("05", AOC2022.Day05.solve)
            ,   ("06", AOC2022.Day06.solve)
            ,   ("07", AOC2022.Day07.solve)
            ,   ("08", AOC2022.Day08.solve)
            ,   ("09", AOC2022.Day09.solve)
            ,   ("10", AOC2022.Day10.solve)
            ,   ("11", AOC2022.Day11.solve)
            ,   ("12", AOC2022.Day12.solve)
            ,   ("13", AOC2022.Day13.solve)
            ,   ("14", AOC2022.Day14.solve)
            ,   ("15", AOC2022.Day15.solve)
            ,   ("16", AOC2022.Day16.solve)
            ,   ("17", AOC2022.Day17.solve)
            ,   ("18", AOC2022.Day18.solve)
            ,   ("19", AOC2022.Day19.solve)
            ,   ("20", AOC2022.Day20.solve)
            ]

solveProblem :: Text -> Text -> RIO SimpleApp ()
solveProblem year name = case Map.lookup name solutions of
    Just solve -> do
        liftIO $ putStrLn $ "Solve day " <> name
        solve =<< readFileUtf8 (Text.unpack $ "./data/" <> year <> "/data" <> name)
    Nothing -> liftIO $ putStrLn $ "Day not implemented: " <> name

main :: IO ()
main = runSimpleApp do
    args <- map Text.pack <$> liftIO getArgs
    traverse_ (solveProblem "2022") if null args then Map.keys solutions else args