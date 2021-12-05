module Day04 (solve) where

import Data.List (transpose)
import Data.List.Split (splitOn)

type Board = [[(Int, Bool)]]

parseData :: [String] -> ([Int], [Board])
parseData (l:_:ls) = (drawn, boards) where
    drawn = map read . splitOn "," $ l
    boards = map (map (map ((,False) . read) . words)) . splitOn [[]] $ ls

hasWon :: Board -> Bool
hasWon board = f board || f (transpose board) where
    f = any (all snd)

play :: Int -> Board -> Board
play x = map (map \(y, b) -> if x == y then (y, True) else (y, b))

score :: Board -> Int
score = sum . map fst . filter (not . snd) . concat

algo1 :: [Int] -> [Board] -> Int
algo1 drawn boards = go drawn boards where
    go [] _ = error "no winner"
    go (x:xs) bs =
        let bs' = map (play x) bs in
        case filter hasWon bs' of
            [] -> go xs bs'
            b:_ -> x * score b

algo2 :: [Int] -> [Board] -> Int
algo2 drawn boards = go drawn boards where
    go [] _ = error "no winner"
    go (x:xs) bs =
        let bs' = map (play x) bs in
        case filter (not . hasWon) bs' of
            [] -> x * score (head bs')
            bs'' -> go xs bs''

solve :: String -> Maybe (Int, Int)
solve s = 
    let (drawn, boards) = parseData . lines $ s in
    Just (algo1 drawn boards, algo2 drawn boards)
