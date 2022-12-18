-- https://adventofcode.com/2022/day/5
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
module Day05 (solve) where
import           RIO hiding (some)
import           RIO.Lens (ix)
import           RIO.List (splitAt, transpose)
import           RIO.List.Partial (head, (!!))
import           Text.Megaparsec (anySingle, between, manyTill, sepEndBy1, sepBy1)
import           Text.Megaparsec.Char (char, eol, string, upperChar)
import           Text.Megaparsec.Char.Lexer (decimal)
import           Util (Parser, aoc)

type Crate = Char
type Ship = [[Crate]]
data Instr = Instr !Int !Int !Int
data Input = Input Ship [Instr]

parser :: Parser Input
parser = Input <$> ship <* garbage <*> instrs where
    ship = map catMaybes . transpose <$> shipLine `sepEndBy1` eol
    shipLine = (crate <|> emptySlot) `sepBy1` char ' '
    crate = Just <$> between (char '[') (char ']') upperChar
    emptySlot = Nothing <$ string "   "
    garbage = manyTill anySingle eol *> eol
    instrs = instr `sepEndBy1` eol
    instr = do
        _ <- string "move "
        move_ <- decimal
        _ <- string " from "
        from <- decimal
        _ <- string " to "
        to_ <- decimal
        pure $ Instr move_ from to_

move :: Bool -> Instr -> Ship -> Ship
move needReverse (Instr nb fromIdx toIdx) ship = ship' where
    (toMove, toKeep) = splitAt nb $ ship !! (fromIdx-1)
    ship' = ship & ix (fromIdx-1) .~ toKeep
                 & ix (toIdx-1) %~ ((if needReverse then reverse else id) toMove ++)

solve' :: Bool -> Input -> String
solve' multi (Input ship instrs) = map head $ foldl' (flip $ move multi) ship instrs

solve :: MonadIO m => Text -> m ()
solve = aoc parser (solve' True) (solve' False)
