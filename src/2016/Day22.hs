-- https://adventofcode.com/2016/day/22
module Day22 (solve) where
import           AOC.Prelude
import           AOC (aoc)
import           AOC.Parser (Parser, decimal, eol, format, hspace, sepEndBy1, skipLine)
import           AOC.List (groupOn)
import           AOC.V2 (V2(..))

data Node = Node { _used, _avail :: !Int}
type Input = [(V2 Int, Node)]

parser :: Parser Input
parser = skipLine *> skipLine *> node `sepEndBy1` eol where
    node = do
        (x, y, _, used, avail, _) <- [format|/dev/grid/node-x{d}-y{d}{d}T{d}T{d}T{d}%|]
        pure (V2 y x, Node used avail)
    d = hspace *> decimal

part1 :: Input -> Int
part1 nodes = aux useds avails len 0 where
    useds = dropWhile (==0) . sort $ map (_used . snd) nodes
    avails = sort $ map (_avail . snd) nodes
    len = length avails
    aux [] _ _ total = total
    aux _ [] _ total = total
    aux (u:us) (a:as) l total | u <= a    = aux us (a:as) l $! total + l 
                              | otherwise = aux (u:us) as (l-1) $! total 

part2 :: Input -> Int
part2 _ = 0

solve :: Text -> IO ()
solve = aoc parser part1 part2