-- https://adventofcode.com/2023/day/18
module AOC2023.Day18 (solve) where
import           AOC.Prelude hiding (Down, Left, Right, tail)
import           Data.Char (isDigit, digitToInt)
import           AOC (aoc)
import           AOC.Parser (Parser, choice, sepEndBy1, eol, count, decimal, hexDigitChar)
import           AOC.V2 (V2(..))
import           AOC.Util (shoelaceFormula)

data Direction = Up | Down | Left | Right
data Instr = Instr !Direction !Int

hexToInt :: String -> Int
hexToInt = foldl' (\acc x -> acc * 16 + hexDigitToInt x) 0
   where hexDigitToInt x
          | isDigit x = digitToInt x
          | otherwise = ord x - ord 'a' + 10

parser :: Parser [(Instr, Instr)]
parser = instr `sepEndBy1` eol where
    instr = do
        dir1 <- direction <* " "
        len1 <- decimal <* " (#" 
        len2 <- hexToInt <$> count 5 hexDigitChar
        dir2 <- direction2 <* ")"
        pure (Instr dir1 len1, Instr dir2 len2)

    direction = choice [Up <$ "U", Down <$ "D", Left <$ "L", Right <$ "R"]
    direction2 = choice [Right <$ "0", Down <$ "1", Left <$ "2", Up <$ "3"]

trenchPoints :: [Instr] -> [V2 Int]
trenchPoints = scanl' go (V2 0 0) where
    go p (Instr dir len) = p + case dir of
        Left -> V2 0 (-len)
        Right -> V2 0 len
        Up -> V2 (-len) 0
        Down -> V2 len 0

-- via Pick theorem and Shoelace Formula
-- https://en.wikipedia.org/wiki/Pick%27s_theorem
-- https://en.wikipedia.org/wiki/Shoelace_formula
solveFor  :: ((Instr, Instr) -> Instr) -> [(Instr, Instr)] -> Int
solveFor f instrs = boundary + interior  where
    instrs' = map f instrs
    doubleArea = shoelaceFormula (trenchPoints instrs')
    boundary = sum [len | Instr _ len <- instrs']
    interior = (doubleArea - boundary) `div` 2 + 1

solve :: Text -> IO ()
solve = aoc parser (solveFor fst) (solveFor snd)