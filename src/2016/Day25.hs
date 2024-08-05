-- https://adventofcode.com/2016/day/25
module Day25 (solve) where
import           AOC.Prelude
import           AOC (aoc)
import           AOC.Parser (Parser, eol, sepEndBy1, signedDecimal, lowerChar, scanf)
import qualified Data.HashMap.Strict as Map
import qualified Data.Vector as V

data Value = Register Char | Constant Int
data Instr = Cpy Value Char | Inc Char | Dec Char | Jnz Value Int | Out Value

parser :: Parser (Vector Instr)
parser = V.fromList <$> instr `sepEndBy1` eol where
    instr = [scanf|$Cpy cpy {val} {c}|]
        <|> [scanf|$Inc inc {c}|]
        <|> [scanf|$Dec dec {c}|]
        <|> [scanf|$Jnz jnz {val} {d}|]
        <|> [scanf|$Out out {val}|]
    c = lowerChar
    d = signedDecimal
    val = Register <$> c <|> Constant <$> d

runProgram :: Vector Instr -> Int -> Bool
runProgram instrs a = go (Map.fromList (zip "abcd" [a, 0, 0, 0])) 0 False Map.empty
    where
    go registers offset lastIsZero prevStates
        | offset >= V.length instrs = False
        | prevStates Map.!? (registers, offset) == Just lastIsZero = True
        | otherwise =
            case instrs V.! offset of
                Cpy val reg -> go (Map.insert reg (eval registers val) registers) (offset + 1) lastIsZero prevStates
                Inc reg -> go (Map.adjust succ reg registers) (offset + 1) lastIsZero prevStates
                Dec reg -> go (Map.adjust pred reg registers) (offset + 1) lastIsZero prevStates
                Jnz val d -> go registers (if eval registers val == 0 then offset + 1 else offset + d) lastIsZero prevStates
                Out val -> 
                    let prevStates' = Map.insert (registers, offset) lastIsZero prevStates
                        out = eval registers val
                    in
                    (out == 1 && lastIsZero || out == 0 && not lastIsZero) && go registers (offset+1) (not lastIsZero) prevStates'
    eval _         (Constant n) = n
    eval registers (Register r) = registers Map.! r

part1 :: Vector Instr -> Maybe Int
part1 instrs = find (runProgram instrs) [0..]

part2 :: Vector Instr -> Int
part2 _ = 0

solve :: Text -> IO ()
solve = aoc parser part1 part2