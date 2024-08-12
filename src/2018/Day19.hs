-- https://adventofcode.com/2018/day/19
module Day19 (solve) where
import           AOC.Prelude
import           AOC (aoc)
import           AOC.Parser (Parser, choice, decimal, eol, sepEndBy1, scanf)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import           Data.Vector.Unboxed ((!), (//))
import           AOC.Number (toDouble)

data Instr =  Seti !Int !Int !Int 
            | Setr !Int !Int !Int 
            | Addi !Int !Int !Int
            | Addr !Int !Int !Int
            | Muli !Int !Int !Int
            | Mulr !Int !Int !Int
            | Eqrr !Int !Int !Int
            | Gtrr !Int !Int !Int
            | Facts !Int !Int !Int -- special instruction to optimize the program

data Program = Program !Int !(Vector Instr)
type Registers = VU.Vector Int

parser :: Parser Program
parser = Program <$> ip <* eol <*> (V.fromList <$> instr `sepEndBy1` eol) where
    ip = "#ip " *> decimal
    instr = choice
        [ [scanf|$Seti seti {d} {d} {d}|]
        , [scanf|$Setr setr {d} {d} {d}|]
        , [scanf|$Addi addi {d} {d} {d}|]
        , [scanf|$Addr addr {d} {d} {d}|]
        , [scanf|$Muli muli {d} {d} {d}|]
        , [scanf|$Mulr mulr {d} {d} {d}|]
        , [scanf|$Eqrr eqrr {d} {d} {d}|]
        , [scanf|$Gtrr gtrr {d} {d} {d}|]
        ]
    d = decimal

go :: Program -> Registers -> Int
go prog@(Program ip instrs) regs =
    case instrs V.!? (regs ! ip) of
        Nothing -> regs ! 0
        Just (Seti a _ c) -> go prog . incIp ip $ regs // [(c, a)]
        Just (Setr a _ c) -> go prog . incIp ip $ regs // [(c, regs ! a)]
        Just (Addi a b c) -> go prog . incIp ip $ regs // [(c, regs ! a + b)]
        Just (Addr a b c) -> go prog . incIp ip $ regs // [(c, regs ! a + regs ! b)]
        Just (Muli a b c) -> go prog . incIp ip $ regs // [(c, regs ! a * b)]
        Just (Mulr a b c) -> go prog . incIp ip $ regs // [(c, regs ! a * regs ! b)]
        Just (Eqrr a b c) -> go prog . incIp ip $ regs // [(c, fromEnum (regs ! a == regs ! b))]
        Just (Gtrr a b c) -> go prog . incIp ip $ regs // [(c, fromEnum (regs ! a > regs ! b))]
        Just (Facts a _ c) -> go prog . incIp ip $ regs // [(c, sumFactors (regs ! a))]

incIp :: Int -> Registers -> Registers
incIp ip regs = regs // [(ip, (regs ! ip) + 1)]

sumFactors :: Int -> Int
sumFactors n = sum [i + q | i <- [1..m], let (q, r) = n `quotRem` i, r == 0] 
                - if m * m  == n then m else 0 where
    m = ceiling . sqrt $ toDouble n

optimize :: Program -> Program
optimize (Program ip instrs) = Program ip instrs' where
    instrs' = case instrs V.! 4 of
        Eqrr _ b _ -> instrs V.// [(1, Facts b 0 0), (2, Seti 10000 0 ip)]
        _ -> error "optimize: unexpected instruction"

part1 :: Program -> Int
part1 prog = go (optimize prog) (VU.replicate 6 0)

part2 :: Program -> Int
part2 prog = go (optimize prog) (VU.fromList [1, 0, 0, 0, 0, 0])

solve :: Text -> IO ()
solve = aoc parser part1 part2