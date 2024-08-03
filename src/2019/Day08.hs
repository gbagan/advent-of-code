module Day08 (solve) where
import           AOC.Prelude hiding (head, last)
import           AOC (aoc)
import           AOC.Parser (Parser, some, digitChar)
import           Data.Char (digitToInt)
import           AOC.List (count, grouped, minimumOn)

parser :: Parser [Int]
parser = some (digitToInt <$> digitChar)

part1 :: [Int] -> Int
part1 xs = count (==1) layer * count (==2) layer where
    chunks = grouped 150 xs
    layer = minimumOn (count (==0)) chunks

part2 :: [Int] -> Int
part2 xs = trace img 0 where
    rawImg = 
        map (fromMaybe 0 . find (/= 2))
        . transpose
        $ grouped 150 xs
    img = intercalate "\n"
        . map (map (bool ' ' '#' . (==1))) 
        $ grouped 25 rawImg

solve :: Text -> IO ()
solve = aoc parser part1 part2