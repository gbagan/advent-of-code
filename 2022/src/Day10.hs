-- https://adventofcode.com/2022/day/10
{-# OPTIONS_GHC -Wno-deprecations #-}
module Day10 (solve) where
import           RIO
import           RIO.List (scanl')
import qualified RIO.Text as Text
import           Data.List.Split (chunksOf, divvy)
import           Text.Megaparsec (sepEndBy1)
import           Text.Megaparsec.Char (eol, string)
import           Util (Parser, aoc', signedDecimal)

data Instr = Noop | Addx Int

parser :: Parser [Instr]
parser = concat <$> instr `sepEndBy1` eol where
    instr = (\v -> [Noop, Addx v]) <$> (string "addx " *> signedDecimal)
        <|> [Noop] <$ string "noop"  

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
part2' xs = trace (Text.pack $ part2 xs) 0

solve :: Text -> RIO env ()
solve = aoc' parser (pure . runInstrs 1) part1 part2'