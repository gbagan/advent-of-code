-- https://adventofcode.com/2015/day/7
module Day07 (solve) where
import AOC.Prelude hiding (Op)
import           AOC (aoc)
import qualified Data.Text as Text
import qualified Data.HashMap.Strict as Map
import            Data.Bits ((.&.), (.|.), shift)
import           AOC.Parser (Parser, some, decimal, lowerChar, sepEndBy1, eol, try)

data Op = And | Or
data ShiftOp = LShift | RShift
data Term = Int !Int 
            | BinOp !Text !Op !Text | ShiftOp !Text !ShiftOp !Int | Not !Text
type Instr = (Term, Text)

parser :: Parser [Instr]
parser = instr `sepEndBy1` eol where
    instr = (,) <$> term <* " -> " <*> label
    label = Text.pack <$> some lowerChar
    term = Int <$> decimal
        <|> Not <$> ("NOT " *> label)
        <|> try (BinOp <$> label <*> op <*> label)
        <|> ShiftOp <$> label <*> shiftOp <*> decimal
    op =  And <$ " AND " <|> Or <$ " OR "
    shiftOp = LShift <$ " LSHIFT " <|> RShift <$ " RSHIFT "

part1 :: [Instr] -> Int
part1 instrs = 0 where
    vars = foldl' go Map.empty instrs
    go vs (term, var) =
        let res = case term of
                    Int n -> n 
                    BinOp var1 And var2 -> vs Map.! var1 .&. vs Map.! var2
                    BinOp var1 Or var2 -> vs Map.! var1 .|. vs Map.! var2
                    ShiftOp var1 LShift n -> vs Map.! var1 `shift` n
                    ShiftOp var1 RShift n -> vs Map.! var1 `shift` (-n)
                    Not var1 -> todo
        in Map.insert var res vs
part2 :: [Instr] -> Int
part2 _ = 0

solve :: Text -> IO ()
solve = aoc parser part1 part2