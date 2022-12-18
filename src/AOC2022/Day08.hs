-- https://adventofcode.com/2022/day/8
module AOC2022.Day08 (solve) where
import           RIO
import           RIO.Partial (fromJust)
import           RIO.Char.Partial (digitToInt)
import           RIO.List.Partial (maximum)
import           Text.Megaparsec (sepEndBy1)
import           Text.Megaparsec.Char (numberChar, eol)
import           Util (Parser, aoc, count)
import           Util.Matrix (Matrix)
import qualified Util.Matrix as M

parser :: Parser (Matrix Int)
parser = M.fromList <$> some (digitToInt <$> numberChar) `sepEndBy1` eol

directions :: [(Int, Int)]
directions = [(0, 1), (1, 0), (0, -1), (-1, 0)]

-- | similar to takeWhile but includes the first element which not satisfies the predicate
takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' p l = l1 ++ take 1 l2 where (l1, l2) = span p l 

-- | return the heights of trees from the position (x, y) in the direction (dx, dy)
heightsInTheDirection :: Matrix Int -> (Int, Int) -> (Int, Int) -> [Int]
heightsInTheDirection vec (x, y) (dx, dy) = map fromJust $ takeWhile isJust [vec M.!? (x+i*dx, y+i*dy) | i <- [1..]]

part1 :: Matrix Int -> Int
part1 vec = count isVisible (M.elemsWithIndex vec) where
    isVisible (x, y, h) = any (isVisible' (x, y) h) directions
    isVisible' xy h dxy = all (<h) (heightsInTheDirection vec xy dxy)

part2 :: Matrix Int -> Int
part2 vec = maximum [viewingDistance (x, y) h | (x, y, h) <- M.elemsWithIndex vec] where
    viewingDistance xy h = product [distance xy h dxy | dxy <- directions]
    distance xy h dxy = length $ takeWhile' (<h) $ heightsInTheDirection vec xy dxy

solve :: MonadIO m => Text -> m ()
solve = aoc parser part1 part2
