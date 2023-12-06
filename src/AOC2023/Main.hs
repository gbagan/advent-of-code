module Main where
import           RIO
import qualified RIO.Map as Map
import qualified AOC2023.Day01 (solve)
import qualified AOC2023.Day02 (solve)
import qualified AOC2023.Day03 (solve)
import qualified AOC2023.Day04 (solve)
import qualified AOC2023.Day05 (solve)
import qualified AOC2023.Day06 (solve)
import           Util (aocMain)

solutions :: Map Text (Text -> RIO SimpleApp ())
solutions = Map.fromList
            [   ("01", AOC2023.Day01.solve)
            ,   ("02", AOC2023.Day02.solve)
            ,   ("03", AOC2023.Day03.solve)
            ,   ("04", AOC2023.Day04.solve)
            ,   ("05", AOC2023.Day05.solve)
            ,   ("06", AOC2023.Day06.solve)
            ]

main :: IO ()
main = aocMain "2023" solutions