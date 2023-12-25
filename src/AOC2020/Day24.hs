-- https://adventofcode.com/2020/day/24
module Day24 (solve) where
import           AOC.Prelude hiding (flip)
import           AOC (aoc)
import           AOC.Parser (Parser, choice, sepEndBy1, eol, some)
import           AOC.V2 (V2(..))
import qualified AOC.HashSet as Set
import qualified AOC.HashMap as Map
import           AOC.Util (times)

parser :: Parser [[V2 Int]]
parser = some direction `sepEndBy1` eol where
    direction = choice [ V2(-1) 0 <$ "w"
                       , V2 1 0 <$ "e"
                       , V2 0 1 <$ "nw"
                       , V2 1 1 <$ "ne"
                       , V2 (-1) (-1) <$ "sw"
                       , V2 0 (-1) <$ "se"
                       ]

flip :: Hashable a => HashSet a -> a -> HashSet a
flip set x | x `Set.member` set = Set.delete x set
           | otherwise = Set.insert x set 

initialSet :: [[V2 Int]] -> HashSet (V2 Int)
initialSet = foldl' flip Set.empty . map (foldl' (+) (V2 0 0))

part1 :: [[V2 Int]] -> Int
part1 = Set.size . initialSet

neighbors :: V2 Int -> [V2 Int]
neighbors (V2 x y) = [ V2 (x-1) y
                    , V2 (x+1) y
                    , V2 x (y+1)
                    , V2 x (y-1)
                    , V2 (x-1) (y-1)
                    , V2 (x+1) (y+1)
                    ]

rule :: Hashable a => HashSet a -> a -> Int -> Bool
rule board pos val = val == 2 || val == 1 && Set.member pos board

step :: HashSet (V2 Int) -> HashSet (V2 Int)
step board = 
    Map.keysSet
    . Map.filterWithKey (rule board)
    . Map.unionsWith (+)
    . map (Map.fromList . map (,1) . neighbors)
    $ Set.toList board

part2 :: [[V2 Int]] -> Int
part2 = Set.size . times 100 step . initialSet

solve :: Text -> IO ()
solve = aoc parser part1 part2