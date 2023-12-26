module Main where
import           AOC.Prelude
import qualified Data.Map.Strict as Map
import           AOC (aocMain)
import qualified Day01 (solve)
import qualified Day02 (solve)
import qualified Day03 (solve)
import qualified Day04 (solve)

solutions :: Map String (Text -> IO ())
solutions = Map.fromList
            [   ("01", Day01.solve)
            ,   ("02", Day02.solve)
            ,   ("03", Day03.solve)
            ,   ("04", Day04.solve)
            ]

main :: IO ()
main = aocMain "2015" solutions