module Day09 (solve) where

import           Data.Maybe (isNothing)
import           Data.List (find, sort)
import           Data.Map (Map, (!))
import qualified Data.Map as Map
import           Util (Point, adjacentPoints, digitToIntMaybe, freqs, parse2dMap)

flow :: Map Point Int -> Map Point (Maybe Point)
flow m = Map.mapWithKey go m where
    go p v = find
                (\p2 -> Map.findWithDefault 10 p2 m < v)
                (adjacentPoints p)

closure :: Map Point (Maybe Point) -> Map Point Point
closure m = cl where
        cl = Map.mapWithKey (\p -> \case
                     Nothing -> p
                     Just c -> cl ! c  
                  ) m

part1 :: Map Point Int -> Int
part1 m = sum . map ((+1) . (m!) . fst) . filter (isNothing . snd) . Map.toList . flow $ m 

part2 :: Map Point Int -> Int
part2 = product . take 3 . reverse . sort . Map.elems . freqs . Map.elems . closure . flow

solve :: String -> Maybe (Int, Int)
solve s = do
    mp <- Map.filter (<9) <$> parse2dMap digitToIntMaybe s
    pure (part1 mp, part2 mp)
