-- https://adventofcode.com/2023/day/12
module AOC2023.Day12 (solve) where
import           AOC.Prelude
import           Data.List (maximum)
import           AOC (aoc)
import           AOC.Parser (Parser, sepEndBy1, some, eol, decimal, hspace)
import qualified Data.Vector as V
import           Data.Array (listArray, range, (!))

data Spring = Operational | Damaged | Unknown deriving (Eq)
data Row = Row [Spring] [Int]

parser :: Parser [Row]
parser = row `sepEndBy1` eol where
    row = Row <$> some spring <* hspace <*> decimal `sepEndBy1` ","
    spring = Operational <$ "." <|> Damaged <$ "#" <|> Unknown <$ "?"

compute :: Row -> Integer
compute (Row springs lengths) = arr ! (0, 0, 0) where
    springs' = springs ++ [Operational]
    vsprings = V.fromList springs'
    springLen = V.length vsprings
    vLengths = V.fromList lengths
    lenLen = V.length vLengths
    arr = listArray bds [
        if pos == springLen then
            if lenPos == lenLen then 1 else 0
        else
            let currentSpring = vsprings V.! pos in
            if lenPos == lenLen then
                if currentSpring == Damaged then 0 else arr ! (pos + 1, lenPos, 0)
            else
                sum $ [Operational, Damaged] <&> \s ->
                    if | currentSpring `notElem` [s, Unknown] -> 0
                       | s == Damaged -> 
                            if | currentLengthPos == vLengths V.! lenPos -> 0
                               | otherwise -> arr ! (pos + 1, lenPos, currentLengthPos + 1)
                       | otherwise -> -- s == Operational
                            if | currentLengthPos == vLengths V.! lenPos -> arr ! (pos + 1, lenPos + 1, 0)
                               | currentLengthPos == 0 -> arr ! (pos + 1, lenPos, 0)
                               | otherwise -> 0
        | (pos, lenPos, currentLengthPos) <- range bds
        ]
    bds = ((0, 0, 0), (springLen, lenLen, maximum lengths))

part1 :: [Row] -> Integer
part1 = sum . map compute

part2 :: [Row] -> Integer
part2 = sum . map (compute . unfold) where
    unfold (Row springs lengths)= Row (intercalate [Unknown] (replicate 5 springs)) (concat (replicate 5 lengths))

solve :: Text -> IO ()
solve = aoc parser part1 part2





{-
part1 :: [Row] -> Int
part1 = sum . map computeRow where
    computeRow (Row springs xs) = count (match xs) (combinations springs)
    combinations [] = [[]]
    combinations (Unknown:xs) = (:) <$> [Operational, Damaged] <*> combinations xs
    combinations (x:xs) = (x:) <$> combinations xs
    match xs springs = (map length . wordsBy (==Operational) $ springs) == xs
-}