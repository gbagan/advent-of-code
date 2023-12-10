module AOC2021.Day11 (solve) where
import           AOC.Prelude hiding (tail)
import           Relude.Unsafe (tail)
import           Data.Char (digitToInt)
import qualified Data.HashMap.Strict as Map
import qualified Data.Set as Set
import           AOC (aoc')
import           AOC.Parser (Parser, digitChar, eol, sepEndBy1, some)
import           AOC.Util (count, kingAdjacentPoints, listTo2dMap)

type Coord = (Int, Int)

parser :: Parser (HashMap Coord Int)
parser = listTo2dMap <$> line `sepEndBy1` eol where
        line = some (digitToInt <$> digitChar)

step :: HashMap Coord Int -> HashMap Coord Int
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

precomp :: HashMap Coord Int -> [HashMap Coord Int]
precomp = iterate' step

part1 :: [HashMap Coord Int] -> Int
part1 = sum . map (count (==0) . Map.elems) . take 100 . tail

part2 :: [HashMap Coord Int] -> Maybe Int
part2 = findIndex (all (==0) . Map.elems)

solve :: Text -> IO ()
solve = aoc' parser (pure . precomp) part1 part2