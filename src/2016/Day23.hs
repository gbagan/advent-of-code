-- https://adventofcode.com/2016/day/23
module Day23 (solve) where
import           AOC.Prelude
import           AOC (aoc')
import           AOC.Parser (Parser, eol, sepEndBy1, signedDecimal, lowerChar, scanf)
import qualified Data.HashMap.Strict as Map
import qualified Data.Vector as V
import           Lens.Micro (ix)
import           Lens.Micro.Mtl ((.=), (+=), (-=), (%=))
import           Lens.Micro.Platform ()
import           Lens.Micro.TH (makeLenses)

data Value = Register Char | Constant Int
data Instr = Inc Char | Dec Char | Tgl Value | Jnz Value Value | Cpy Value Char | Mul Value Value Char

data PState = PState { _program :: !(Vector Instr), _registers :: !(HashMap Char Int) }

makeLenses ''PState

parser :: Parser [Instr]
parser = optimizeCode <$> instr `sepEndBy1` eol where
    instr = [scanf|$Cpy cpy {val} {c}|]
        <|> [scanf|$Inc inc {c}|]
        <|> [scanf|$Dec dec {c}|]
        <|> [scanf|$Tgl tgl {val}|]
        <|> [scanf|$Jnz jnz {val} {val}|]
    c = lowerChar
    val = Register <$> c <|> Constant <$> signedDecimal

runProgram :: HashMap Char Int -> Vector Instr  -> Int
runProgram initialRegs initialInstrs = evalState (go 0) (PState initialInstrs initialRegs)
    where
    go !offset = do
        PState instrs regs <- get
        if offset >= V.length instrs 
            then pure $! regs Map.! 'a'
            else case instrs V.! offset of
                Cpy val reg -> do
                    registers . ix reg .= eval regs val 
                    go (offset + 1)
                Inc reg -> do
                    registers . ix reg += 1
                    go (offset + 1)
                Dec reg -> do
                    registers . ix reg -= 1
                    go (offset + 1)
                Jnz val val' -> do
                    go (if eval regs val == 0 then offset + 1 else offset + eval regs val')
                Tgl val -> do
                    let v = eval regs val
                    toggle (offset + v)
                    go (offset + 1)
                Mul a b c -> do
                    registers . ix c .= eval regs a * eval regs b
                    go (offset + 1)
    
eval :: HashMap Char Int -> Value -> Int
eval _ (Constant n) = n
eval regs (Register r) = regs Map.! r

toggle :: Int -> State PState ()
toggle offset =
    program . ix offset %= \case
        Inc x              -> Dec x
        Dec x              -> Inc x
        Tgl (Register x)   -> Inc x
        Jnz x (Register y) -> Cpy x y
        Cpy x y            -> Jnz x (Register y)
        _ -> error "invalid toggle"

optimizeCode :: [Instr] -> [Instr]
optimizeCode [] = []
optimizeCode (Cpy _ _ : Inc _ : Dec _ : Jnz _ (Constant(-2)) : Dec _ : Jnz _ (Constant(-5)) : xs) =
    Mul (Register 'b') (Register 'd') 'a' 
    : Cpy (Constant 0) 'c'
    : Cpy (Constant 0) 'd'
    : Jnz (Constant 0) (Constant 0)
    : Jnz (Constant 0) (Constant 0)
    : Jnz (Constant 0) (Constant 0)
    : optimizeCode xs
optimizeCode (x:xs) = x : optimizeCode xs

part1 :: Vector Instr -> Int
part1 = runProgram (Map.fromList [('a', 7), ('b', 0), ('c', 0), ('d', 0)])

part2 :: Vector Instr -> Int
part2 = runProgram (Map.fromList [('a', 12), ('b', 0), ('c', 0), ('d', 0)])

solve :: Text -> IO ()
solve = aoc' parser (Just . V.fromList . optimizeCode) part1 part2