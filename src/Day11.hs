module Day11 (solve) where
import           Data.Char (digitToInt)
import           Data.List (iterate', findIndex, foldl')
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe (fromJust)
import qualified Data.Set as Set
import           Text.Megaparsec (sepEndBy1, some)
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as L
import           Util (Parser, Point, aocTemplate, count, digitToIntMaybe, kingAdjacentPoints, listTo2dMap)

parser :: Parser (Map Point Int)
parser = listTo2dMap <$> line `sepEndBy1` P.eol where
        line = some (digitToInt <$> P.digitChar)

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

part1 :: Map Point Int -> Int
part1 = sum . map (count (==0) . Map.elems) . take 100 . tail . iterate step

part2 :: Map Point Int -> Maybe Int
part2 = findIndex (all (==0) . Map.elems) . iterate step

solve :: String -> IO ()
solve = aocTemplate parser (Just . part1) part2