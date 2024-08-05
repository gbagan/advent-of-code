module Main where
import           AOC.Prelude
import qualified Data.Map.Strict as Map
import           AOC (aocMain)
import qualified Day21 (solve)
import qualified Day22 (solve)
import qualified Day23 (solve)
import qualified Day24 (solve)
import qualified Day25 (solve)

solutions :: Map String (Text -> IO ())
solutions = Map.fromList
            [ ("21", Day21.solve)
            , ("22", Day22.solve)
            , ("23", Day23.solve)
            , ("24", Day24.solve)
            , ("25", Day25.solve)
            ]

main :: IO ()
main = aocMain "2017" solutions