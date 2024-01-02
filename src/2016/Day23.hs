-- https://adventofcode.com/2016/day/23
module Day23 (solve) where
import           AOC.Prelude
import           AOC (aoc)
import           AOC.Parser (Parser, eol, sepEndBy1, signedDecimal, lowerChar, format)
import qualified Data.HashMap.Strict as Map
import qualified Data.Vector as V
import           Lens.Micro (ix)
import           Lens.Micro.Mtl ((.=), (+=), (-=))
import           Lens.Micro.Platform ()
import           Lens.Micro.TH (makeLenses)

data Value = Register Char | Constant Int
data Instr = Inc Char | Dec Char | Tgl Char | Jnz Value Value | Cpy Value Value 

data PState = PState { _program :: !(Vector Instr), _registers :: !(HashMap Char Int) }

makeLenses ''PState

parser :: Parser (Vector Instr)
parser = V.fromList <$> instr `sepEndBy1` eol where
    instr = [format|$Cpy cpy {val} {val}|]
        <|> [format|$Inc inc {c}|]
        <|> [format|$Dec dec {c}|]
        <|> [format|$Tgl tgl {c}|]
        <|> [format|$Jnz jnz {val} {val}|]
    c = lowerChar
    val = Register <$> c <|> Constant <$> signedDecimal

runProgram ::  HashMap Char Int -> Vector Instr  -> Int
runProgram initialRegs initialInstrs = evalState (go 0) (PState initialInstrs initialRegs)
    where
    go offset = do
        PState instrs regs <- get
        if offset >= V.length instrs 
            then pure $! regs Map.! 'a'
            else case instrs V.! offset of
                Cpy val (Register reg) -> do
                    registers . ix reg .= eval regs val 
                    go (offset + 1)
                Inc reg -> do
                    registers . ix reg += 1
                    go (offset + 1)
                Dec reg -> do
                    registers . ix reg -= 1
                    go (offset + 1)
                Jnz val (Constant d) -> do
                    go (if eval regs val == 0 then offset + 1 else offset + d)
                _ -> go (offset + 1)
        where
        eval _ (Constant n) = n
        eval regs (Register r) = regs Map.! r

part1 :: Vector Instr -> Int
part1 = runProgram (Map.fromList $ map (,0) "abcd")

part2 :: Vector Instr -> Int
part2 = runProgram (Map.fromList $ zip "abcd" [0, 0, 1, 0])

solve :: Text -> IO ()
solve = aoc parser part1 part2