-- https://adventofcode.com/2022/day/5
module Day05 (solve) where
import           RIO hiding (some)
import           RIO.Lens (ix)
import           RIO.List (splitAt, transpose)
import           RIO.List.Partial (head, (!!))
import           Text.Megaparsec (anySingle, between, manyTill, sepEndBy1, sepBy1)
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as L
import           Util (Parser, aoc)

type Crate = Char
type Ship = [[Crate]]
type Instr = (Int, Int, Int)
data Input = Input Ship [Instr]

parser :: Parser Input
parser = Input <$> ship <* garbage <*> instrs where
    ship = map catMaybes . transpose <$> shipLine `sepEndBy1` P.eol
    shipLine = (crate <|> emptySlot) `sepBy1` P.char ' '
    crate = Just <$> between (P.char '[') (P.char ']') P.upperChar
    emptySlot = Nothing <$ P.string "   "
    garbage = manyTill anySingle P.eol *> P.eol
    instrs = instr `sepEndBy1` P.eol
    instr = (,,) <$> (P.string "move " *> L.decimal) <*> (P.string " from " *> L.decimal) <*> (" to " *> L.decimal)

move :: Bool -> Instr -> Ship -> Ship
move multi (nb, fromIdx, toIdx) ship = ship' where
    (toMove, toKeep) = splitAt nb $ ship !! (fromIdx-1)
    ship' =  ship & ix (fromIdx-1) .~ toKeep
                  & ix (toIdx-1) %~ ((if multi then id else reverse) toMove ++)

solve' :: Bool -> Input -> String
solve' multi (Input ship instrs) = map head $ foldl' (flip $ move multi) ship instrs

solve :: (HasLogFunc env) => Text -> RIO env ()
solve = aoc parser (solve' False) (solve' True)
