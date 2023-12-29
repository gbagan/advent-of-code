module Main where
import           AOC.Prelude
import qualified Data.Map.Strict as Map
import           AOC (aocMain)
import qualified Day01 (solve)

solutions :: Map String (Text -> IO ())
solutions = Map.fromList
            [ ("01", Day01.solve)
            ]

main :: IO ()
main = aocMain "2016" solutions