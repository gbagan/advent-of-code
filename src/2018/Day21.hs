-- https://adventofcode.com/2018/day/21
module Day21 (solve) where
import           AOC.Prelude
import           AOC (aoc_)
import           AOC.Parser (Parser, choice, decimal, eol, sepEndBy1, scanf)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import           Data.Vector.Unboxed ((!), (//))
import           Data.Bits ((.&.), (.|.))
import           Data.List ((!!))
import           AOC.List (findDuplicate', headMaybe)

data Instr =  Seti !Int !Int !Int 
            | Setr !Int !Int !Int 
            | Addi !Int !Int !Int
            | Addr !Int !Int !Int
            | Muli !Int !Int !Int
            | Mulr !Int !Int !Int
            | Bani !Int !Int !Int
            | Bori !Int !Int !Int
            | Eqri !Int !Int !Int
            | Eqrr !Int !Int !Int
            | Gtir !Int !Int !Int
            | Gtrr !Int !Int !Int

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
        , [scanf|$Bani bani {d} {d} {d}|]
        , [scanf|$Bori bori {d} {d} {d}|]
        , [scanf|$Eqrr eqrr {d} {d} {d}|]
        , [scanf|$Eqri eqri {d} {d} {d}|]
        , [scanf|$Gtir gtir {d} {d} {d}|]
        , [scanf|$Gtrr gtrr {d} {d} {d}|]
        ]
    d = decimal

go :: Program -> Registers -> [Int]
go prog@(Program ip instrs) regs =
    case instrs V.!? (regs ! ip) of
        Nothing -> []
        Just (Seti a _ c) -> go prog . incIp ip $ regs // [(c, a)]
        Just (Setr a _ c) -> go prog . incIp ip $ regs // [(c, regs ! a)]
        Just (Addi a b c) -> go prog . incIp ip $ regs // [(c, regs ! a + b)]
        Just (Addr a b c) -> go prog . incIp ip $ regs // [(c, regs ! a + regs ! b)]
        Just (Muli a b c) -> go prog . incIp ip $ regs // [(c, regs ! a * b)]
        Just (Mulr a b c) -> go prog . incIp ip $ regs // [(c, regs ! a * regs ! b)]
        Just (Bani a b c) -> go prog . incIp ip $ regs // [(c, regs ! a .&. b)]
        Just (Bori a b c) -> go prog . incIp ip $ regs // [(c, regs ! a .|. b)]
        Just (Eqri a b c) -> go prog . incIp ip $ regs // [(c, fromEnum (regs ! a == b))]
        Just (Eqrr a b c) -> if b == 0 then regs ! a : next else next
            where next = go prog . incIp ip $ regs // [(c, fromEnum (regs ! a == regs ! b))]
        Just (Gtir a b c) -> go prog . incIp ip $ regs // [(c, fromEnum (a > regs ! b))]
        Just (Gtrr a b c) -> go prog . incIp ip $ regs // [(c, fromEnum (regs ! a > regs ! b))]

incIp :: Int -> Registers -> Registers
incIp ip regs = regs // [(ip, (regs ! ip) + 1)]

solve' :: Program -> Maybe (Int, Int)
solve' prog = do 
    let values = go prog (VU.replicate 6 0)
    p1 <- headMaybe values
    (_, j, _) <- findDuplicate' values
    let p2 = values !! (j-1)
    Just (p1, p2)

solve :: Text -> IO ()
solve = aoc_ parser solve'