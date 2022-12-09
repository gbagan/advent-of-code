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
import System.Environment (getArgs)

solutions :: Map String (Text -> RIO SimpleApp ())
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
            ]

solveProblem :: String -> RIO SimpleApp ()
solveProblem name = case Map.lookup name solutions of
    Just solve -> do
        logInfo $ "Solve day " <> display (Text.pack name)
        solve =<< readFileUtf8 ("./data/data" <> name)
    Nothing -> logInfo $ "Day not implemented: " <> display (Text.pack name)

main :: IO ()
main = runSimpleApp do
    args <- liftIO getArgs
    traverse_ solveProblem if null args then Map.keys solutions else args