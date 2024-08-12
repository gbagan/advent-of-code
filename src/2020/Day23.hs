-- https://adventofcode.com/2020/day/23
module Day23 (solve) where
import           AOC.Prelude hiding (head, last)
import           Control.Monad.ST (ST, runST)
import           Data.Char (digitToInt)
import qualified Data.Vector.Unboxed.Mutable as V
import           AOC (aoc)
import           AOC.List (head, last, drop1)
import           AOC.Parser (Parser, some, digitChar)
import           AOC.Util (timesM)

parser :: Parser [Int]
parser = some (digitToInt <$> digitChar)

-- we represent the circle as a vector such that vec[cup] is the next cup in the circle

makeCircle :: Int -> [Int] -> ST r (V.MVector r Int)
makeCircle n xs = do
    let m = length xs
    vec <- V.generate n succ
    zipWithM_ (V.write vec) xs (drop1 xs)
    if m == n
        then V.write vec (last xs) (head xs)
        else do
            V.write vec (last xs) m
            V.write vec (n-1) (head xs)
    pure vec

step :: V.MVector r Int -> Int -> ST r Int
step vec currentCup = do
    let size = V.length vec
        top = size - 1 
    
    -- the 3 next cups
    cup1 <- V.read vec currentCup
    cup2 <- V.read vec cup1
    cup3 <- V.read vec cup2
    
    let pred' n = if n == 0 then top else n-1
    let destinationCup n 
            | p /= cup1 && p /= cup2 && p /= cup3 = p
            | otherwise = destinationCup p
            where p = pred' n
    let dest = destinationCup currentCup
    -- currentCup -> cup1 -> cup2 -> cup3 -> nextCup -> ... -> dest -> ...
    -- becomes
    -- currentCup -> nextCup -> ... -> dest -> cup1 -> cup2 -> cup3 -> ...
    nextCup <- V.read vec cup3
    V.write vec currentCup nextCup
    V.write vec dest cup1
    V.write vec cup3 =<< V.read vec dest
    
    pure nextCup

-- extract the n cups after of the cup x in the circle
successors :: V.MVector r Int -> Int -> Int ->  ST r [Int]
successors _ 0 _ = pure []
successors vec m x = do 
    y <- V.read vec x
    ys <- successors vec (m-1) y
    pure (y : ys)

solveFor :: Int -> Int -> Int -> [Int] -> [Int]
solveFor size nbIters nbSucc xs = runST do
    let xs' = map pred xs
    vec <- makeCircle size xs'
    _ <- timesM nbIters (step vec) (head xs')
    map succ <$> successors vec nbSucc 0

part1 :: [Int] -> String
part1 = concatMap show . solveFor 9 100 8

part2 :: [Int] -> Int
part2 = product . solveFor 1_000_000 10_000_100 2

solve :: Text -> IO ()
solve = aoc parser part1 part2