-- https://adventofcode.com/2015/day/7
module Day07 (solve) where
import AOC.Prelude hiding (Op)
import           AOC (aoc)
import qualified Data.Text as Text
import qualified Data.HashMap.Strict as Map
import qualified Data.HashMap.Lazy as LMap
import            Data.Bits ((.&.), (.|.), complement, shiftL, shiftR)
import           AOC.Parser (Parser, some, decimal, lowerChar, sepEndBy1, eol, try)

data Op = And | Or | LShift | RShift
data Term = Signal !Word16 | Wire !Text
data Gate = ConstGate !Term | Gate2 !Term !Op !Term | Not !Term
type Circuit = HashMap Text Gate

parser :: Parser Circuit
parser = Map.fromList <$> instr `sepEndBy1` eol where
    instr = flip (,) <$> expr <* " -> " <*> label
    label = Text.pack <$> some lowerChar
    expr = Not <$> ("NOT " *> wire)
        <|> try (Gate2 <$> wire <*> op <*> wire)
        <|> ConstGate <$> wire
    wire = Signal <$> decimal <|> Wire <$> label
    op =  And <$ " AND " 
        <|> Or <$ " OR " 
        <|> LShift <$ " LSHIFT "
        <|> RShift <$ " RSHIFT "

evalCircuit :: Circuit -> HashMap Text Word16
evalCircuit circuit = eval where
    eval = circuit & LMap.map \case
        ConstGate wire        -> evalWire wire
        Gate2 wire1 op wire2 -> evalOp op (evalWire wire1) (evalWire wire2)
        Not wire              -> complement (evalWire wire)
    evalWire (Signal n) = n
    evalWire (Wire v) = eval Map.! v
    evalOp And n m = n .&. m
    evalOp Or n m = n .|. m
    evalOp LShift n m = n `shiftL` fromIntegral m
    evalOp RShift n m = n `shiftR` fromIntegral m

part1 :: Circuit -> Maybe Word16
part1 = (Map.!? "a") . evalCircuit

part2 :: Circuit -> Maybe Word16
part2 circuit = do
    evalA <- evalCircuit circuit Map.!? "a"
    let circuit' = Map.insert "b" (ConstGate (Signal evalA)) circuit
    evalCircuit circuit' Map.!? "a"

solve :: Text -> IO ()
solve = aoc parser part1 part2