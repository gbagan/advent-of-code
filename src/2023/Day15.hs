-- https://adventofcode.com/2023/day/15
module Day15 (solve) where
import           AOC.Prelude
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Sequence as Seq
import           AOC (aoc)
import           AOC.Parser (Parser, sepEndBy1, some, lowerChar, decimal)

data Operation = Remove | Insert !Int
data Step = Step !String !Operation
type Box = Seq (String, Int)
type Boxes = IntMap Box

stepToString :: Step -> String
stepToString (Step str Remove) = str ++ "-"
stepToString (Step str (Insert n)) = str ++ "=" ++ show n

parser :: Parser [Step]
parser = step `sepEndBy1` "," where
    step = Step <$> some lowerChar <*> operation
    operation = Remove <$ "-" <|> Insert <$> ("=" *> decimal)

hash :: String -> Int
hash = foldl' go 0 where
    go acc ch = (acc + ord ch) * 17 `mod` 256

insertLens :: String -> Int -> Box -> Box
insertLens label len boxes = case Seq.findIndexL ((== label) . fst) boxes of
    Nothing -> boxes Seq.|> (label, len)
    Just idx -> Seq.update idx (label, len) boxes

deleteLens :: String -> Box -> Box
deleteLens label = Seq.filter ((/= label) . fst)

part1 :: [Step] -> Int
part1 = sum . map (hash . stepToString)

hashmapStep :: Boxes -> Step -> Boxes
hashmapStep boxes (Step label instr) = case instr of
    Remove -> IntMap.adjust (deleteLens label) k boxes 
    Insert len -> IntMap.adjust (insertLens label len) k boxes
    where k = hash label

focusingPower :: Boxes -> Int
focusingPower boxes = sum [ (i+1) * j * len 
                          | (i, box) <- IntMap.toList boxes
                          , (j, (_, len)) <- zip [1..] (toList box)
                          ]
part2 :: [Step] -> Int
part2 = focusingPower . foldl' hashmapStep initialBoxes where
    initialBoxes = IntMap.fromList (zip [0..] (replicate 256 Seq.Empty))

solve :: Text -> IO ()
solve = aoc parser part1 part2