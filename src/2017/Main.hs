module Main where
import           AOC.Prelude
import qualified Data.Map.Strict as Map
import           AOC (aocMain)
import qualified Day21 (solve)

solutions :: Map String (Text -> IO ())
solutions = Map.fromList
            [ ("21", Day21.solve)
            ]

main :: IO ()
main = aocMain "2017" solutions