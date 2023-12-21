module Day15 (solve) where
import           AOC.Prelude
import           Data.Char (digitToInt)
import           Data.Massiv.Array (Matrix, (!), (!?), fromLists', makeArray, U, Comp(Seq), Ix2(..), Sz(..))
import           AOC (aoc)
import           AOC.V2 (V2(..), adjacent, toIx2)
import           AOC.Parser (Parser, sepEndBy1, some, digitChar, eol)
import           AOC.Search (dijkstra)

type Coord = V2 Int

parser :: Parser (Matrix U Int)
parser = fromLists' Seq <$> line `sepEndBy1` eol where
    line = some (digitToInt <$> digitChar)

neighbors :: Matrix U Int -> Coord -> [(Coord, Int)]
neighbors mat v = mapMaybe (\p -> (p,) <$> (mat !? toIx2 p)) (adjacent v)

part1 :: Matrix U Int -> Maybe Int
part1 mp = dijkstra (neighbors mp) (== V2 99 99) (V2 0 0) 

duplicateGrid :: Matrix U Int -> Matrix U Int
duplicateGrid m = makeArray Seq (Sz2 500 500) \(Ix2 i j) -> nm $ (m ! Ix2 (i `mod` 100) (j `mod` 100)) + i `div` 100 + j `div` 100
    where nm x = if x > 9 then x - 9 else x

part2 :: Matrix U Int -> Maybe Int
part2 mat = dijkstra (neighbors mat') (== V2 499 499) (V2 0 0)  where
        mat' = duplicateGrid mat

solve :: Text -> IO ()
solve = aoc parser part1 part2