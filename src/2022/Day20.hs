-- https://adventofcode.com/2022/day/20
module Day20 (solve) where
import           AOC.Prelude
import           Data.List ((!!))
import           Data.Maybe (fromJust)
import qualified Data.Sequence as Seq
import           Data.Sequence (Seq(..), (><))
import           AOC (aoc)
import           AOC.Parser (Parser, sepEndBy1, eol, signedDecimal)

parser :: Parser [Int]
parser = signedDecimal `sepEndBy1` eol

rotate :: Int -> Seq a -> Seq a
rotate n s = s2 >< s1
    where (s1, s2) = Seq.splitAt (n `mod` Seq.length s) s

-- rotate the seq such that the first element is x
rotateOn :: (Eq a) => a -> Seq a -> Seq a
rotateOn x s = rotate idx s where
    idx = fromJust $ Seq.findIndexL (==x) s

step :: (Int, Int) -> Seq (Int, Int) -> Seq (Int, Int)
step x@(_, v) s = x :<| (rotate v . Seq.drop 1 . rotateOn x $ s)

iteration :: Seq (Int, Int) -> Seq (Int, Int) -> Seq (Int, Int)
iteration s s' = foldl' (flip step) s' s

solveFor :: Int -> Int -> [Int] -> Int
solveFor key nbIters l = a + b + c where
    s = Seq.fromList $ zip [0..] $ map (*key) l
    x0 = fromJust $ find ((==0) . snd) s
    s' = rotateOn x0 $ iterate' (iteration s) s !! nbIters
    n = Seq.length s
    (_, a) = Seq.index s' (1000 `mod` n)
    (_, b) = Seq.index s' (2000 `mod` n)
    (_, c) = Seq.index s' (3000 `mod` n)

solve :: Text -> IO ()
solve = aoc parser (solveFor 1 1) (solveFor 811589153 10)