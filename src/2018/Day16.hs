-- https://adventofcode.com/2018/day/16
module Day16 (solve) where
import           AOC.Prelude
import           AOC (aoc)
import           AOC.List (count, headMaybe)
import           AOC.Parser (Parser, sepEndBy1, eol, decimal, scanf)
import qualified Data.Vector as V
import           Data.Vector ((!), (//))
import           Data.Bits ((.|.), (.&.))
import           AOC.Graph (perfectMatchings)
import qualified Data.IntSet as Set
import qualified Data.IntMap.Strict as Map

type Registers = V.Vector Int
data Instr = Instr !Int !Int !Int !Int
data Sample = Sample Registers Instr Registers

parser :: Parser ([Sample], [Instr])
parser = (,) <$> samples <* eol <* eol <*> instrs where
    samples = sample `sepEndBy1` eol
    sample = do
        (a, b, c, d) <- [scanf|Before: [{i}, {i}, {i}, {i}]|] <* eol
        instr <- instruction <* eol
        (a', b', c', d') <- [scanf|After:  [{i}, {i}, {i}, {i}]|] <* eol
        pure $ Sample (V.fromList [a, b, c, d]) instr (V.fromList [a', b', c', d'])
    instrs = instruction `sepEndBy1` eol
    instruction = [scanf|$Instr {i} {i} {i} {i}|]
    i = decimal

opCodes :: Vector (Instr -> Registers -> Registers)
opCodes = V.fromList
    [ \(Instr _ a b c) v -> v V.// [(c, v ! a + v ! b)]  -- addr
    , \(Instr _ a b c) v -> v V.// [(c, v ! a + b)] -- addi
    , \(Instr _ a b c) v -> v V.// [(c, v ! a * v ! b)]  -- addr
    , \(Instr _ a b c) v -> v V.// [(c, v ! a * b)] -- addi
    , \(Instr _ a b c) v -> v V.// [(c, v ! a .&. v ! b)]  -- banr
    , \(Instr _ a b c) v -> v V.// [(c, v ! a .&. b)] -- bani
    , \(Instr _ a b c) v -> v V.// [(c, v ! a .|. v ! b)]  -- borr
    , \(Instr _ a b c) v -> v V.// [(c, v ! a .|. b)] -- bori
    , \(Instr _ a _ c) v -> v V.// [(c, v ! a)]  -- setr
    , \(Instr _ a _ c) v -> v V.// [(c, a)]  -- seti
    , \(Instr _ a b c) v -> v V.// [(c, fromEnum (a     >  v ! b))] -- gtir
    , \(Instr _ a b c) v -> v V.// [(c, fromEnum (v ! a >  b))]  -- gtri
    , \(Instr _ a b c) v -> v V.// [(c, fromEnum (v ! a >  v ! b))]  -- gtrr
    , \(Instr _ a b c) v -> v V.// [(c, fromEnum (a     == v ! b))] -- eqir
    , \(Instr _ a b c) v -> v V.// [(c, fromEnum (v ! a == b))]  -- eqri
    , \(Instr _ a b c) v -> v V.// [(c, fromEnum (v ! a == v ! b))]  -- eqrr
    ]

matchedOpCodes :: Sample -> (Int, IntSet)
matchedOpCodes (Sample before instr@(Instr i _ _ _) after) = (i, matched) where
    matched = Set.fromList [ j
                           | (j, opcode) <- zip [0..] (V.toList opCodes)
                           , opcode instr before == after
                           ]

part1 :: ([Sample], [Instr]) -> Int
part1 (samples, _) = count go samples where
    go sample = Set.size (snd (matchedOpCodes sample)) >= 3

part2 :: ([Sample], [Instr]) -> Maybe Int
part2 (samples, instrs) = do
    let graph =
            map (second Set.toList)
            . Map.toList
            . Map.fromListWith Set.intersection
            $ map matchedOpCodes samples    
    matching <- headMaybe (perfectMatchings graph)
    let sortedOpCodes = opCodes // map (second (opCodes !)) matching
    pure $ foldl' (go sortedOpCodes) (V.replicate 4 0) instrs ! 0
    where
    go codes regs instr@(Instr i _ _ _) = (codes ! i) instr regs

solve :: Text -> IO ()
solve = aoc parser part1 part2