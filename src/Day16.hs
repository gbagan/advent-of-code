module Day16 (solve) where
import Util (Parser, aocTemplate)

type Input = ()

parser :: Parser Input
parser = return ()

part1 :: Input -> Maybe Int
part1 _ = Nothing

part2 :: Input -> Maybe Int
part2 _ = Nothing

solve :: String -> IO ()
solve = aocTemplate parser part1 part2