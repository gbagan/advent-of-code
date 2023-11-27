module Main where
import           RIO
import qualified RIO.Map as Map
import qualified AOC2023.Day01 (solve)
import           Util (aocMain)

solutions :: Map Text (Text -> RIO SimpleApp ())
solutions = Map.fromList
            [   ("01", AOC2023.Day01.solve)
            ]

main :: IO ()
main = aocMain "2023" solutions