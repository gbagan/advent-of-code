module Main where
import           RIO
import qualified RIO.Text as Text
import qualified RIO.Map as Map
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
import qualified Day15 (solve)
import qualified Day16 (solve)
import qualified Day17 (solve)
import qualified Day18 (solve)
import qualified Day19 (solve)
import qualified Day20 (solve)
import qualified Day21 (solve)
import qualified Day22 (solve)
import qualified Day23 (solve)
import qualified Day24 (solve)
import qualified Day25 (solve)
import System.Environment (getArgs)

solutions :: Map Text (Text -> RIO SimpleApp ())
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
            ,   ("15", Day15.solve)
            ,   ("16", Day16.solve)
            ,   ("17", Day17.solve)
            ,   ("18", Day18.solve)
            ,   ("19", Day19.solve)
            ,   ("20", Day20.solve)
            ,   ("21", Day21.solve)
            ,   ("22", Day22.solve)
            ,   ("23", Day23.solve)
            ,   ("24", Day24.solve)
            ,   ("25", Day25.solve)
            ]

solveProblem :: Text -> RIO SimpleApp ()
solveProblem name = case Map.lookup name solutions of
    Just solve -> do
        logInfo $ "Solve day " <> display name
        s <- readFileUtf8 (Text.unpack $ "./data/data" <> name)
        solve s
    Nothing -> logInfo $ "Day not implemented: " <> display name

main :: IO ()
main = runSimpleApp do
    args <- map Text.pack <$> liftIO getArgs
    traverse_ solveProblem if null args then Map.keys solutions else args