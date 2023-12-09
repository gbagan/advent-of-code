-- https://adventofcode.com/2022/day/10
{-# OPTIONS_GHC -Wno-deprecations #-}
module AOC2022.Day10 (solve) where
import           Relude hiding (unlines)
import           Data.List (unlines)
import           Data.List.Split (chunksOf, divvy)
import           AOC (aoc')
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
part1 = sum . concat . divvy 1 40 . drop 19 . zipWith (*) [1..] 

part2 :: [Int] -> String
part2 = unlines . map (zipWith drawPixel [0..]) . chunksOf 40 where
    drawPixel i x | abs (i - x) <= 1 = '#'
                  | otherwise = '.'

part2' :: [Int] -> Int
part2' xs = trace (part2 xs) 0

solve :: Text -> IO ()
solve = aoc' parser (pure . runInstrs 1) part1 part2'