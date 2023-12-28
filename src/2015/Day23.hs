-- https://adventofcode.com/2015/day/23
module Day23 (solve) where
import           AOC.Prelude hiding (mod)
import           AOC (aoc)
import           AOC.Parser (Parser, choice, decimal, sepEndBy1, eol)
import qualified Data.Vector as V

data Register = A | B
type Offset = Int
data Instr = 
      Hlf !Register 
    | Tpl !Register 
    | Inc !Register
    | Jmp !Offset
    | Jie !Register !Offset
    | Jio !Register !Offset

parser :: Parser (Vector Instr)
parser = V.fromList <$> instr `sepEndBy1` eol where
    instr = choice
        [ Hlf <$> ("hlf " *> register)
        , Tpl <$> ("tpl " *> register)
        , Inc <$> ("inc " *> register)
        , Jmp <$> ("jmp " *> offset)
        , Jie <$> ("jie " *> register) <* ", " <*> offset 
        , Jio <$> ("jio " *> register) <* ", " <*> offset 
        ]
    offset = "+" *> decimal <|> negate <$> ("-" *> decimal)
    register = A <$ "a" <|> B <$ "b"

simulate :: Vector Instr -> Int -> (Int, Int) -> (Int, Int)
simulate instrs offset registers
    | offset == V.length instrs = registers
    | otherwise = case instrs V.! offset of
        Hlf r   -> mod (`quot` 2) r
        Tpl r   -> mod (*3) r
        Inc r   -> mod (+1) r
        Jmp o   -> jmp (const True) A o
        Jie r o -> jmp even r o
        Jio r o -> jmp (==1) r o
    where
    ap A = first
    ap B = second
    sel A = fst
    sel B = snd
    mod f r = simulate instrs (offset+1) (ap r f registers)
    jmp f r o = simulate
                    instrs 
                    (if f (sel r registers) then offset+o else offset + 1)
                    registers

part1 :: Vector Instr -> Int
part1 instrs = snd $ simulate instrs 0 (0, 0) 

part2 :: Vector Instr -> Int
part2 instrs = snd $ simulate instrs 0 (1, 0) 

solve :: Text -> IO ()
solve = aoc parser part1 part2