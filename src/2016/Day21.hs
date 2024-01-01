-- https://adventofcode.com/2016/day/21
module Day21 (solve) where
import           AOC.Prelude
import           Data.List ((!!))
import           AOC (aoc)
import           AOC.Parser (Parser, decimal, letterChar, sepEndBy1, eol, format, optional)

data Operation = SwapLetter Char Char
               | SwapPosition Int Int
               | RotateLeft Int
               | RotateRight Int
               | RotatePosition Char
               | Reverse Int Int
               | Move Int Int

parser :: Parser [Operation]
parser = operation `sepEndBy1` eol where
    operation = [format|$SwapPosition swap position {d} with position {d}|]
            <|> [format|$SwapLetter swap letter {c} with letter {c}|]
            <|> [format|$RotateLeft rotate left {d} step|] <* optional "s"
            <|> [format|$RotateRight rotate right {d} step|] <* optional "s"
            <|> [format|$RotatePosition rotate based on position of letter {c}|]
            <|> [format|$Reverse reverse positions {d} through {d}|]
            <|> [format|$Move move position {d} to position {d}|]
    c = letterChar
    d = decimal

doOperation :: String -> Operation -> String
doOperation s = \case
    SwapLetter c1 c2 -> map f s where
        f c | c == c1   = c2
            |  c == c2  = c1
            | otherwise = c
    SwapPosition p1 p2 -> zipWith f [0..] s where
        f idx c | idx == p1 = c2
                | idx == p2 = c1
                | otherwise = c
        c1 = s !! p1
        c2 = s !! p2
    RotateLeft n -> doOperation s $ RotateRight (-n)
    RotateRight n -> ys ++ xs where
        m = length s
        n' = m - (n `mod` m)
        (xs,ys) = splitAt n' s
    RotatePosition c -> case elemIndex c s of
        Nothing -> s
        Just idx -> doOperation s $ RotateRight (1 + idx + (if idx >= 4 then 1 else 0))
    Reverse p1 p2 -> xs ++ reverse ys ++ zs where
        (xs,xs') = splitAt p1 s
        (ys,zs) = splitAt (p2-p1+1) xs'
    Move p1 p2 -> case splitAt p1 s of
        (_, []) -> s
        (xs, x:xs') ->
            let (ys, zs) = splitAt p2 (xs++xs') in
            ys ++ [x] ++ zs

undoOperation :: String -> Operation -> String
undoOperation s op = doOperation s case op of
    RotateLeft n -> RotateRight n
    RotateRight n -> RotateLeft n
    RotatePosition c -> case elemIndex c s of
        Nothing -> error "reverseOperation: cannot happen"
        Just idx -> let n = idx `div` 2 + if odd idx || idx == 0 then 1 else 5
                    in RotateLeft n
    Move p1 p2 -> Move p2 p1
    _ -> op

part1 :: [Operation] -> String
part1 = foldl' doOperation "abcdefgh"

part2 :: [Operation] -> String
part2 = foldl' undoOperation "fbgdceah". reverse

solve :: Text -> IO ()
solve = aoc parser part1 part2