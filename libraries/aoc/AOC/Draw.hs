module AOC.Draw where
import           AOC.Prelude hiding (unlines)
import           AOC.V2 (V2(..))
import           AOC.Area (Area(..))
import qualified AOC.Area as Area
import qualified Data.HashMap.Strict as Map
import           Data.List (unlines)

drawPicture :: HashMap (V2 Int) Char -> String
drawPicture pixels = unlines [[Map.findWithDefault ' ' (V2 x y) pixels | x <- [xmin..xmax]] | y <- [ymin..ymax]]
    where Area xmin ymin xmax ymax = Area.boundingBox (Map.keys pixels)

drawPoints :: [V2 Int] -> String
drawPoints coords = drawPicture (Map.fromList [(c,'#') | c <- coords])