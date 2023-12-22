-- https://adventofcode.com/2023/day/12
module Day12 (solve) where
import           AOC.Prelude
import           AOC (aoc)
import           AOC.Parser (Parser, sepEndBy1, some, eol, decimal, hspace)
import qualified Data.Massiv.Array as A
import           Data.Massiv.Array ((!), B, BL, U, Comp(Seq), Sz(..), Ix2(..))

data Spring = Operational | Damaged | Unknown deriving (Eq)
type Row = ([Spring], [Int])

parser :: Parser [Row]
parser = row `sepEndBy1` eol where  
    row = (,) <$> some spring <* hspace <*> decimal `sepEndBy1` ","
    spring = Operational <$ "." <|> Damaged <$ "#" <|> Unknown <$ "?"

countArrangements :: Row -> Integer
countArrangements (springs, groups) = arr ! Ix2 0 0 where
    vsprings = A.fromList @B Seq (springs ++ [Operational])
    Sz springsLength = A.size vsprings
    vGroups = A.fromList @U Seq groups
    Sz groupsLength = A.size vGroups
    nextOperational = A.makeArray @BL Seq (Sz springsLength) \i ->
        if (vsprings ! i) == Operational then i else nextOperational ! (i+1)
    arr = A.makeArray @BL Seq (Sz2 (springsLength+1) (groupsLength+1)) \(Ix2 pos groupPos) ->
        if pos == springsLength then
            if groupPos == groupsLength then 1 else 0
        else
            let nextOp = nextOperational ! pos
                pos' =  pos + (vGroups ! groupPos)
                x = if (vsprings ! pos) /= Damaged
                        then arr ! Ix2 (pos + 1) groupPos 
                        else 0
                y = if groupPos < groupsLength && nextOp >= pos' && (vsprings ! pos') /= Damaged
                        then arr ! Ix2 (pos' + 1) (groupPos + 1)
                        else 0
            in x + y

solveFor :: (Row -> Row) -> [Row] -> Integer
solveFor f = sum . map (countArrangements . f)

unfold :: Row -> Row
unfold = bimap (intercalate [Unknown] . replicate 5) (concat . replicate 5)

solve :: Text -> IO ()
solve = aoc parser (solveFor id) (solveFor unfold)