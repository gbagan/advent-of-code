-- https://adventofcode.com/2023/day/15
module AOC2023.Day15 (solve) where
import           AOC.Prelude
import qualified Data.IntMap.Strict as IntMap
import qualified Data.HashMap.Strict.InsOrd as HMap 
import           AOC (aoc)
import           AOC.Parser (Parser, sepEndBy1, some, lowerChar, decimal)

data Instr = Dash String | Equals String Int
type Box = HMap.InsOrdHashMap String Int
type Boxes = IntMap Box

instrToString :: Instr -> String
instrToString (Dash str) = str ++ "-"
instrToString (Equals str n) = str ++ "=" ++ show n

parser :: Parser [Instr]
parser = instr `sepEndBy1` "," where
    instr = do
        str <- some lowerChar
        Dash str <$ "-" <|> Equals str <$> ("=" *> decimal)

hash :: String -> Int
hash = foldl' go 0 where
    go acc ch = (acc + ord ch) * 17 `mod` 256

part1 :: [Instr] -> Int
part1 = sum . map (hash . instrToString)

hashmapStep :: Boxes -> Instr -> Boxes
hashmapStep boxes = \case
    Dash label ->
        let k = hash label in
        IntMap.adjust (HMap.delete label) k boxes 
    Equals label len ->
        let k = hash label in
        IntMap.adjust (HMap.insert label len) k boxes

focusingPower :: Boxes -> Int
focusingPower boxes = sum [ (i+1) * j * len 
                          | (i, box) <- IntMap.toList boxes
                          , (j, (_, len)) <- zip [1..] (HMap.toList box)
                          ]
part2 :: [Instr] -> Int
part2 = focusingPower . foldl' hashmapStep initialBoxes where
    initialBoxes = IntMap.fromList (zip [0..] (replicate 256 HMap.empty))

solve :: Text -> IO ()
solve = aoc parser part1 part2