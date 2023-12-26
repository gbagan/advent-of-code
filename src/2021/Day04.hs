module Day04 (solve) where
import           AOC.Prelude hiding (head)
import           Data.List (head)
import           AOC (aoc)
import           AOC.Parser (Parser, char, decimal, eol, hspace, sepEndBy1, sepBy1)

type Board = [[(Int, Bool)]]
data Input = Input [Int] [Board]

parser :: Parser Input
parser = Input <$> draw <* eol <* eol <*> boards where
    draw =  decimal `sepBy1` char ','
    boards = board `sepEndBy1` eol
    board = line `sepEndBy1` eol
    line =  hspace *> ((,False) <$> decimal) `sepEndBy1` hspace

hasWon :: Board -> Bool
hasWon board = f board || f (transpose board) where
    f = any (all snd)

play :: Int -> Board -> Board
play x = map (map \(y, b) -> (y, b || x == y))

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

solve :: Text -> IO ()
solve = aoc parser part1 part2
