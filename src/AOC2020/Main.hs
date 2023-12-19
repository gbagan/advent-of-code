module Main where
import           AOC.Prelude
import qualified Data.Map.Strict as Map
import           AOC (aocMain)
import qualified AOC2020.Day01 (solve)
import qualified AOC2020.Day02 (solve)
import qualified AOC2020.Day03 (solve)
import qualified AOC2020.Day04 (solve)
import qualified AOC2020.Day05 (solve)
import qualified AOC2020.Day06 (solve)
import qualified AOC2020.Day07 (solve)
import qualified AOC2020.Day08 (solve)
import qualified AOC2020.Day09 (solve)
import qualified AOC2020.Day10 (solve)
import qualified AOC2020.Day11 (solve)
import qualified AOC2020.Day12 (solve)
import qualified AOC2020.Day13 (solve)
import qualified AOC2020.Day14 (solve)

solutions :: Map String (Text -> IO ())
solutions = Map.fromList
            [   ("01", AOC2020.Day01.solve)
            ,   ("02", AOC2020.Day02.solve)
            ,   ("03", AOC2020.Day03.solve)
            ,   ("04", AOC2020.Day04.solve)
            ,   ("05", AOC2020.Day05.solve)
            ,   ("06", AOC2020.Day06.solve)
            ,   ("07", AOC2020.Day07.solve)
            ,   ("08", AOC2020.Day08.solve)
            ,   ("09", AOC2020.Day09.solve)
            ,   ("10", AOC2020.Day10.solve)
            ,   ("11", AOC2020.Day11.solve)
            ,   ("12", AOC2020.Day12.solve)
            ,   ("13", AOC2020.Day13.solve)
            ,   ("14", AOC2020.Day14.solve)
            ]

main :: IO ()
main = aocMain "2020" solutions