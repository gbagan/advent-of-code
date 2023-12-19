-- https://adventofcode.com/2020/day/14
module AOC2020.Day14 (solve) where
import           AOC.Prelude
import           AOC (aoc)
-- import           Data.Bits ((.&.), (.|.), xor, complement, shiftL, shiftR, popCount)
import qualified Data.IntMap.Strict as Map
import           AOC.Parser (Parser, sepEndBy1, choice, count, eol, decimal)
import           AOC.Util (binToInt)

data Bit = Zero | One | X
data Instr = Write !Int !Int | Mask ![Bit]
type Input = [Instr]

intToBin :: Int -> [Bool]
intToBin = reverse . go (36 :: Int) where
    go 0 _ = []
    go c n = (n `mod` 2 == 1) : go (c-1) (n `div` 2)

parser :: Parser Input
parser = (mask <|> write) `sepEndBy1` eol where    
    mask = Mask <$> ("mask = " *> count 36 bit)
    write = Write <$> ("mem[" *> decimal) <* "] = " <*> decimal
    bit = choice [Zero <$ "0", One <$ "1", X <$ "X"]

applyMask :: [Bit] -> Int -> Int
applyMask mask n = binToInt $ zipWith applyToBit mask (intToBin n) where
    applyToBit One _ = True
    applyToBit Zero _ = False
    applyToBit X b = b
    
part1 :: Input -> Int
part1 instrs = sum . Map.elems . fst $ foldl' go (Map.empty, []) instrs where
    go (memory, mask) = \case 
        Mask mask' -> (memory, mask')
        Write address n -> (Map.insert address (applyMask mask n) memory, mask)

maskCombinations :: [Bit] -> [[Bit]]
maskCombinations [] = [[]]
maskCombinations (Zero : bits) = (X:) <$> maskCombinations bits
maskCombinations (One : bits) = (One:) <$> maskCombinations bits
maskCombinations (X : bits) = (:) <$> [Zero, One] <*> maskCombinations bits


part2 :: Input -> Int
part2 instrs = sum . Map.elems . fst $ foldl' go (Map.empty, []) instrs where
    go (memory, masks) = \case 
        Mask mask' -> (memory, maskCombinations mask')
        Write address n -> (memory', masks) where
            memory' = foldl' go' memory masks
            go' mem mask = Map.insert (applyMask mask address) n mem

solve :: Text -> IO ()
solve = aoc parser part1 part2