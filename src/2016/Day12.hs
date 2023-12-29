-- https://adventofcode.com/2016/day/12
module Day12 (solve) where
import           AOC.Prelude
import           AOC (aoc)
import           AOC.Parser (Parser, eol, sepEndBy1, signedDecimal, lowerChar, format)
import qualified Data.HashMap.Strict as Map
import qualified Data.Vector as V

data Value = Register Char | Constant Int
data Instr = Cpy Value Char | Inc Char | Dec Char | Jnz Value Int 

parser :: Parser (Vector Instr)
parser = V.fromList <$> instr `sepEndBy1` eol where
    instr = [format|$Cpy cpy {val} {c}|]
        <|> [format|$Inc inc {c}|]
        <|> [format|$Dec dec {c}|]
        <|> [format|$Jnz jnz {val} {d}|]
    c = lowerChar
    d = signedDecimal
    val = Register <$> c <|> Constant <$> d

runProgram ::  HashMap Char Int -> Vector Instr  -> Int
runProgram initialRegs instrs = go initialRegs 0
    where
    go registers offset
        | offset >= V.length instrs = registers Map.! 'a'
        | otherwise =
            case instrs V.! offset of
                Cpy val reg -> go (Map.insert reg (eval val) registers) (offset + 1) 
                Inc reg -> go (Map.adjust succ reg registers) (offset + 1)
                Dec reg -> go (Map.adjust pred reg registers) (offset + 1) 
                Jnz val d -> go registers (if eval val == 0 then offset + 1 else offset + d)
        where
        eval (Constant n) = n
        eval (Register r) = registers Map.! r

part1 :: Vector Instr -> Int
part1 = runProgram (Map.fromList $ map (,0) "abcd")

part2 :: Vector Instr -> Int
part2 = runProgram (Map.fromList $ zip "abcd" [0, 0, 1, 0])

solve :: Text -> IO ()
solve = aoc parser part1 part2