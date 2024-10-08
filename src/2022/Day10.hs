-- https://adventofcode.com/2022/day/10
module Day10 (solve) where
import           AOC.Prelude hiding (head, unlines)
import           Data.List (head, unlines)
import           AOC (aocIO')
import           AOC.List (grouped)
import           AOC.Parser (Parser, sepEndBy1, eol, signedDecimal)

data Instr = Noop | Addx Int

parser :: Parser [Instr]
parser = concat <$> instr `sepEndBy1` eol where
    instr = (\v -> [Noop, Addx v]) <$> ("addx " *> signedDecimal)
        <|> [Noop] <$ "noop"

runInstrs :: Int -> [Instr] -> [Int]
runInstrs = scanl' (flip runInstr) where
    runInstr Noop = id
    runInstr (Addx v) = (+v)

part1 :: [Int] -> Int
part1 = sum . map head . grouped 40 . drop 19 . zipWith (*) [1..] 

part2 :: [Int] -> String
part2 = unlines . map (zipWith drawPixel [0..]) . grouped 40 where
    drawPixel i x | abs (i - x) <= 1 = '#'
                  | otherwise = '.'

part2' :: [Int] -> IO Int
part2' xs = putStr (part2 xs) $> 0

solve :: Text -> IO ()
solve = aocIO' parser (pure . runInstrs 1) (pure . part1) part2'