-- https://adventofcode.com/2016/day/16
module Day16 (solve) where
import           AOC.Prelude
import           AOC (aoc)
import           AOC.Parser (Parser, some)
import qualified Data.Vector.Unboxed as V

parser :: Parser (V.Vector Bool)
parser = V.fromList <$> some (False <$ "0" <|> True <$ "1")

dragonCurve :: Int -> V.Vector Bool -> V.Vector Bool
dragonCurve n vec 
    | V.length vec >= n = V.take n vec
    | otherwise         = dragonCurve n $
                            vec <> V.singleton (False) <> V.reverse (V.map not vec)

checksum :: V.Vector Bool -> String
checksum vec | even (V.length vec) = checksum (aux vec)
            | otherwise            = map (bool '0' '1') (V.toList vec)
    where
    aux v = V.generate (V.length v `quot`2) \i ->
                v V.! (2*i) == v V.! (2*i+1)

solveFor :: Int-> V.Vector Bool -> String
solveFor n = checksum . dragonCurve n

solve :: Text -> IO ()
solve = aoc parser (solveFor 272) (solveFor 35_651_584)