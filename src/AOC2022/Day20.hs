-- https://adventofcode.com/2022/day/20
module AOC2022.Day20 (solve) where
import           RIO
import           RIO.List (find, iterate)
import           RIO.Partial (fromJust)
import           RIO.List.Partial ((!!))
import qualified RIO.Seq as Seq
import           RIO.Seq (Seq(..), (><))
import           Text.Megaparsec (sepEndBy1)
import           Text.Megaparsec.Char (eol)
import           Util (Parser, aoc)
import           Util.Parser (signedDecimal)

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

solve' :: Int -> Int -> [Int] -> Int
solve' key nbIters l = a + b + c where
    s = Seq.fromList $ zip [0..] $ map (*key) l
    x0 = fromJust $ find ((==0) . snd) s
    s' = rotateOn x0 $ iterate (iteration s) s !! nbIters
    n = Seq.length s
    (_, a) = Seq.index s' (1000 `mod` n)
    (_, b) = Seq.index s' (2000 `mod` n)
    (_, c) = Seq.index s' (3000 `mod` n)

solve :: MonadIO m => Text -> m ()
solve = aoc parser (solve' 1 1) (solve' 811589153 10)