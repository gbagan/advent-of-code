{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TupleSections #-}

module Main where

import Data.List (transpose)

type Board = [[(Int, Bool)]]

splitWhen :: (a -> Bool) -> [a] -> [[a]]
splitWhen p l =  case dropWhile p l of
                      [] -> []
                      l' -> w : splitWhen p l''
                            where (w, l'') = break p l'

parseData :: [String] -> ([Int], [Board])
parseData (l:_:ls) = (drawn, boards) where
    drawn = map read . splitWhen (== ',') $ l
    boards = map (map (map ((,False) . read) . words)) . splitWhen null $ ls

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

main = do
    (drawn, boards) <- parseData . lines <$> readFile "data04"
    print $ algo1 drawn boards
    print $ algo2 drawn boards
