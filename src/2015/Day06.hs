-- https://adventofcode.com/2015/day/6
module Day06 (solve) where
import           AOC.Prelude hiding (toList)
import           AOC (aoc)
import           AOC.Parser (Parser, sepEndBy1, eol, hspace, decimal)
import           Control.Monad.ST (ST)
import           Data.Massiv.Array (U, Sz(..), Ix2(..), toList)
import           Data.Massiv.Array.Mutable
import           AOC.List (count)

data Command = Toggle | On | Off deriving (Show)
data Instruction = Instruction !Command !Int !Int !Int !Int deriving (Show)

parser :: Parser [Instruction]
parser = instruction `sepEndBy1` eol where
    instruction = do
        cmd <- On <$ "turn on" <|> Off <$ "turn off"<|> Toggle <$ "toggle"
        x1 <- hspace *> decimal
        y1 <- "," *> decimal
        x2 <- " through " *> decimal
        y2 <- "," *> decimal
        pure $ Instruction cmd x1 y1 x2 y2

runInstruction :: MArray r U Ix2 Bool -> Instruction -> ST r ()
runInstruction arr (Instruction cmd x1 y1 x2 y2) = do
    for_ [x1..x2] \i ->
        for_ [y1..y2] \j ->
            case cmd of
                On     -> write_ arr (Ix2 i j) True
                Off    -> write_ arr (Ix2 i j) False
                Toggle -> modify_ arr (pure . not) (Ix2 i j)

part1 :: [Instruction] -> Int
part1 instructions = count id . toList $ createArrayST_ @U (Sz2 1000 1000) \arr -> do
    for_ instructions (runInstruction arr)

runInstruction2 :: MArray r U Ix2 Int -> Instruction -> ST r ()
runInstruction2 arr (Instruction cmd x1 y1 x2 y2) = do
    for_ [x1..x2] \i ->
        for_ [y1..y2] \j ->
            case cmd of
                On     -> modify_ arr (pure . succ) (Ix2 i j)
                Off    -> modify_ arr (pure . max 0 . pred) (Ix2 i j)
                Toggle -> modify_ arr (pure . (+2)) (Ix2 i j)

part2 :: [Instruction] -> Int
part2 instructions = sum . toList $ createArrayST_ @U (Sz2 1000 1000) \arr -> do
    for_ instructions (runInstruction2 arr)

solve :: Text -> IO ()
solve = aoc parser part1 part2