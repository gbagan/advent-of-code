-- https://adventofcode.com/2017/day/24
module Day24 (solve) where
import           AOC.Prelude
import           AOC (aoc)
import           AOC.Parser (Parser, decimal, eol, sepEndBy1)
import qualified Data.HashSet as Set
import           Data.List (maximum)
import           AOC.List (maximumOn)

parser :: Parser (HashSet (Int, Int))
parser = Set.fromList <$> edge `sepEndBy1` eol where
    edge = (,) <$> decimal <* "/" <*> decimal

bridges :: HashSet (Int, Int) -> [(Int, Int)]
bridges = go 0 0 0 where
    go !len !strength !v !pieces =
        if | (v, v) `Set.member` pieces -> go (len+1) (strength + 2 * v) v (Set.delete (v, v) pieces)
           | null adequatePieces        -> [(len, strength)]
           | otherwise                  -> do
                                            piece <- adequatePieces
                                            let v' = otherExtremity v piece
                                            go (len+1) (strength+v+v') v' (Set.delete piece pieces)
        where adequatePieces = [ (u, u') | (u, u') <- Set.toList pieces, u == v || u' == v ]

otherExtremity :: Int -> (Int, Int) -> Int
otherExtremity u (v, v') | u == v    = v'
                         | otherwise = v

part1 :: HashSet (Int, Int) -> Int
part1 = maximum . map snd . bridges

part2 :: HashSet (Int, Int) -> Int
part2 = snd . maximumOn fst . bridges

solve :: Text -> IO ()
solve = aoc parser part1 part2