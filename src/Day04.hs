module Day04 (solve) where

import           Data.List (transpose)
import           Text.Megaparsec (sepEndBy1, sepBy1)
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as L
import           Util (Parser, aocTemplate)

type Board = [[(Int, Bool)]]
data Input = Input [Int] [Board]

parser :: Parser Input
parser = Input <$> draw <* P.eol <* P.eol <*> boards where
    draw =  L.decimal `sepBy1` P.char ','
    boards = board `sepEndBy1` P.eol
    board = line `sepEndBy1` P.eol
    line =  P.hspace *> ((,False) <$> L.decimal) `sepEndBy1` P.hspace1

hasWon :: Board -> Bool
hasWon board = f board || f (transpose board) where
    f = any (all snd)

play :: Int -> Board -> Board
play x = map (map \(y, b) -> if x == y then (y, True) else (y, b))

score :: Board -> Int
score = sum . map fst . filter (not . snd) . concat

part1 :: Input -> Int
part1 (Input draw boards) = go draw boards where
    go [] _ = 0
    go (x:xs) bs =
        let bs' = map (play x) bs in
        case filter hasWon bs' of
            [] -> go xs bs'
            b:_ -> x * score b

part2 :: Input -> Int
part2 (Input draw boards) = go draw boards where
    go [] _ = 0
    go (x:xs) bs =
        let bs' = map (play x) bs in
        case filter (not . hasWon) bs' of
            [] -> x * score (head bs')
            bs'' -> go xs bs''

solve :: String -> IO ()
solve = aocTemplate parser pure (pure . part1) (pure . part2)
