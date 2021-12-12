module Day11 (solve) where
import           Data.List (iterate', findIndex, foldl')
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe (fromJust)
import qualified Data.Set as Set
import           Util (Point, digitToIntMaybe, kingAdjacentPoints, parse2dMap)

step :: Map Point Int -> Map Point Int
step mp = mp3 where
    mp1 = Map.map (+1)  mp
    stack = map fst . filter ((>9) . snd) . Map.toList $ mp1
    mp2 = go stack Set.empty mp1
    mp3 = Map.map (\v -> if v > 9 then 0 else v) mp2
    go [] _ mp' = mp'
    go (x:xs) flashed mp' =
            case Map.lookup x mp' of
                Nothing -> go xs flashed mp'
                Just v -> let mp'' = Map.insert x (v+1) mp' in
                            if v >= 9 && not (Set.member x flashed) then 
                                go (kingAdjacentPoints x ++ xs) (Set.insert x flashed) mp''
                            else
                                go xs flashed mp''

stepList :: Map Point Int -> [Map Point Int]
stepList = iterate step

part1 :: [Map Point Int] -> Int
part1 = sum . map (length . filter (==0) . Map.elems) . take 100 . tail

part2 :: [Map Point Int] -> Int
part2 = fromJust . findIndex (all (==0) . Map.elems)

solve :: String -> Maybe (Int, Int)
solve s = do
    mp <- parse2dMap digitToIntMaybe s
    let l = stepList mp
    pure (part1 l, part2 l)