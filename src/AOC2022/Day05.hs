-- https://adventofcode.com/2022/day/5
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
module AOC2022.Day05 (solve) where
import           Relude hiding (head, some)
import           Relude.Unsafe (head, (!!))
import           Lens.Micro ((%~), (.~), ix)
import           AOC (aoc)
import           AOC.Parser (Parser, anySingle, between, manyTill, sepEndBy1, sepBy1, eol, upperChar, decimal)

type Crate = Char
type Ship = [[Crate]]
data Instr = Instr !Int !Int !Int
data Input = Input Ship [Instr]

parser :: Parser Input
parser = Input <$> ship <* garbage <*> instrs where
    ship = map catMaybes . transpose <$> shipLine `sepEndBy1` eol
    shipLine = (crate <|> emptySlot) `sepBy1` " "
    crate = Just <$> between "[" "]" upperChar
    emptySlot = Nothing <$ "   "
    garbage = manyTill anySingle eol *> eol
    instrs = instr `sepEndBy1` eol
    instr = do
        _ <- "move "
        move_ <- decimal
        _ <- " from "
        from <- decimal
        _ <- " to "
        to_ <- decimal
        pure $ Instr move_ from to_

move :: Bool -> Instr -> Ship -> Ship
move needReverse (Instr nb fromIdx toIdx) ship = ship' where
    (toMove, toKeep) = splitAt nb $ ship !! (fromIdx-1)
    ship' = ship & ix (fromIdx-1) .~ toKeep
                 & ix (toIdx-1) %~ ((if needReverse then reverse else id) toMove ++)

solveFor :: Bool -> Input -> String
solveFor multi (Input ship instrs) = map head $ foldl' (flip $ move multi) ship instrs

solve :: Text -> IO ()
solve = aoc parser (solveFor True) (solveFor False)
