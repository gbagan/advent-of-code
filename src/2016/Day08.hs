-- https://adventofcode.com/2016/day/8
module Day08 (solve) where
import           AOC.Prelude hiding (toList)
import           AOC (aoc)
import           Control.Monad.ST (ST)
import           Data.Massiv.Array hiding (map)
import           AOC.Parser (Parser, eol, sepEndBy1, decimal, format)
import           System.IO.Unsafe (unsafePerformIO)
import           AOC.List (count)

data Instr = Rect !Int !Int | RotateRow !Int !Int | RotateCol !Int !Int 

parser :: Parser [Instr]
parser = instr `sepEndBy1` eol where
    instr = [format|$Rect rect {decimal}x{decimal}|]
        <|> [format|$RotateRow rotate row y={decimal} by {decimal}|]
        <|> [format|$RotateCol rotate column x={decimal} by {decimal}|]

runInstr :: MArray r U Ix2 Bool -> Instr -> ST r ()
runInstr mat = \case 
    Rect cols rows ->
        for_ [0 .. rows-1] $ \i ->
            for_ [0 .. cols-1] $ \j ->
                write_ mat (Ix2 i j) True
    RotateCol x n -> rotate mat (`Ix2` x) height n
    RotateRow y n -> rotate mat (Ix2 y) width n
    where Sz2 height width = sizeOfMArray mat

rotate :: (Manifest r a, Index ix) => MArray s r ix a -> (Int -> ix) -> Int -> Int -> ST s ()
rotate mat f len d = do
    for_ [0..g-1] \i -> do
        let j = (i-d) `mod` len
        v <- readM mat (f j)
        rotateAux i j
        write_ mat (f i) v
    where
    g = gcd len d

    rotateAux end i
        | i == end = pure ()
        | otherwise = do
            let k = (i - d) `mod` len
            v <- readM mat (f k)
            write_ mat (f i) v
            rotateAux end k

{-# INLINE rotate #-}

computeScreen :: [Instr] -> Matrix U Bool
computeScreen instrs =
    createArrayST_ @U (Sz2 6 50) \m -> do
        for_ instrs (runInstr m) 

part1 :: [Instr] -> Int
part1 = count id . toList . computeScreen

part2 :: [Instr] -> Int
part2 instrs = unsafePerformIO do
    let text = map (map (bool ' ' '#')) . toLists2 . computeScreen $ instrs
    traverse_ putStrLn text
    pure 0

solve :: Text -> IO ()
solve = aoc parser part1 part2