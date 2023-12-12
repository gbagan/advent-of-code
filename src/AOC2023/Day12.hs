-- https://adventofcode.com/2023/day/12
module AOC2023.Day12 (solve) where
import           AOC.Prelude
import           AOC (aoc)
import           AOC.Parser (Parser, sepEndBy1, some, eol, decimal, hspace)
import qualified Data.Vector as V
import           Data.Array (listArray, range, (!))

data Spring = Operational | Damaged | Unknown deriving (Eq, Show)
type Row = ([Spring], [Int])

parser :: Parser [Row]
parser = row `sepEndBy1` eol where  
    row = (,) <$> some spring <* hspace <*> decimal `sepEndBy1` ","
    spring = Operational <$ "." <|> Damaged <$ "#" <|> Unknown <$ "?"

countArrangements :: Row -> Integer
countArrangements (springs, groups) = arr ! (0, 0) where
    vsprings = V.fromList (springs ++ [Operational])
    springsLength = V.length vsprings
    vGroups = V.fromList groups
    groupsLength = V.length vGroups
    nextOperational = V.generate springsLength \i ->
        if vsprings V.! i == Operational then i else nextOperational V.! (i+1)
    arr = listArray bounds [
        if pos == springsLength then
            if groupPos == groupsLength then 1 else 0
        else
            let nextOp = nextOperational V.! pos
                pos' =  pos + vGroups V.! groupPos
                x = if vsprings V.! pos /= Damaged then arr ! (pos + 1, groupPos) else 0
                y = if groupPos < groupsLength && nextOp >= pos' && vsprings V.! pos' /= Damaged
                    then arr ! (pos' + 1, groupPos + 1)
                    else 0
            in x + y
        | (pos, groupPos) <- range bounds
        ]
    bounds = ((0, 0), (springsLength, groupsLength))

solveFor :: (Row -> Row) -> [Row] -> Integer
solveFor f = sum . map (countArrangements . f)

unfold :: Row -> Row
unfold = bimap (intercalate [Unknown] . replicate 5) (concat . replicate 5)

solve :: Text -> IO ()
solve = aoc parser (solveFor id) (solveFor unfold)



{-
part1 :: [Row] -> Int
part1 = sum . map countArrangements where
    countArrangements (springs, groups) = count (match groups) (combinations springs)
    combinations [] = [[]]
    combinations (Unknown:xs) = (:) <$> [Operational, Damaged] <*> combinations xs
    combinations (x:xs) = (x:) <$> combinations xs
    match groups springs = (map length . wordsBy (==Operational) $ springs) == groups
-}