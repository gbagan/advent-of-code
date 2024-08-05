-- https://adventofcode.com/2017/day/23
module Day23 (solve) where
import           AOC.Prelude
import           AOC (aoc)
import           AOC.Parser (Parser, eol, sepEndBy1, signedDecimal, lowerChar, scanf)
import           AOC.List (count)
import           AOC.Number (toDouble)
import qualified Data.HashMap.Strict as Map
import qualified Data.Vector as V

data Value = Register Char | Constant Int
data Instr = Set Char Value | Sub Char Value | Mul Char Value | Jnz Value Int 

parser :: Parser (Vector Instr)
parser = V.fromList <$> instr `sepEndBy1` eol where
    instr = [scanf|$Set set {c} {val}|]
        <|> [scanf|$Sub sub {c} {val}|]
        <|> [scanf|$Mul mul {c} {val}|]
        <|> [scanf|$Jnz jnz {val} {d}|]
    c = lowerChar
    d = signedDecimal
    val = Register <$> c <|> Constant <$> d

runProgram ::  HashMap Char Int -> Vector Instr  -> Int
runProgram initialRegs instrs = go initialRegs 0 0
    where
    go registers offset nbMuls
        | offset >= V.length instrs = nbMuls
        | otherwise = case instrs V.! offset of
            Set reg val -> go (Map.insert reg (eval val) registers) (offset + 1) nbMuls
            Sub reg val -> go (Map.adjust (subtract (eval val)) reg registers) (offset + 1) nbMuls
            Mul reg val -> go (Map.adjust (* eval val) reg registers) (offset + 1) (nbMuls+1)
            Jnz val d -> go registers (if eval val == 0 then offset + 1 else offset + d) nbMuls
        where
        eval (Constant n) = n
        eval (Register r) = Map.findWithDefault 0 r registers

part1 :: Vector Instr -> Int
part1 = runProgram (Map.fromList (map (,0) "abcdefgh")) 

isPrime :: Int -> Bool
isPrime n = n >= 2 && and [n `rem` i /= 0 | i <- [2..floor . sqrt $ toDouble n]]

part2 :: Vector Instr -> Int
part2 instrs = case (instrs V.! 0, instrs V.! 4, instrs V.!5, instrs V.! 7) of
    (Set _ (Constant x), Mul _ (Constant y), Sub _ (Constant z), Sub _ (Constant t)) ->
        count (not . isPrime) [b,b+17..c]
        where
        b = x * y - z
        c = b - t
    _ -> error "part2"

solve :: Text -> IO ()
solve = aoc parser part1 part2