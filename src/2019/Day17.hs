-- https://adventofcode.com/2019/day/17
module Day17 (solve) where
import           AOC.Prelude hiding (last, lines, unlines)
import           AOC (aoc_)
import           AOC.Parser (Parser, signedDecimal, sepBy1) 
import           Data.List (last, lines, unlines)
import           AOC.List (count, flattenWithIndex', headMaybe)
import           AOC.IntCode (runProgram)
import           Data.Massiv.Array (Matrix, (!?), fromLists', U, Comp(Seq))
import           AOC.V2 (V2(..), adjacent, north, turnLeft, turnRight, toIx2)

data Turn = TurnRight | TurnLeft deriving (Eq, Show)
data Instr = Move !Turn !Int | Function !Int deriving (Eq, Show)

parser :: Parser [Int]
parser = signedDecimal `sepBy1` ","

getWalk :: Matrix U Char -> V2 Int -> [Instr]
getWalk grid = go north where
    go !dir !pos =
        if leftTile /= Just '#' && rightTile /= Just '#'
            then []
            else Move turn n : go dir' pos''
        where
        leftTile = grid !? toIx2 (pos + turnLeft dir)
        rightTile = grid !? toIx2 (pos + turnRight dir)
        (pos', dir', turn) =
            if leftTile == Just '#'
                then (pos + turnLeft dir, turnLeft dir, TurnLeft)
                else (pos + turnRight dir, turnRight dir, TurnRight)
        walk = takeWhile (\p -> (grid !? toIx2 p) == Just '#') . scanl' (+) pos' $ repeat dir'
        n = length walk
        pos'' = last walk

suffixAfter :: Eq a => [a] -> [a] -> Maybe [a]
suffixAfter [] ys = Just ys
suffixAfter _ [] = Nothing
suffixAfter (x:xs) (y:ys) | x == y = suffixAfter xs ys
                          | otherwise = Nothing

replaceWith :: Eq a => [a] -> [a] -> [a] -> [a]
replaceWith _ _ [] = []
replaceWith a b xs | Just ys <- suffixAfter a xs = b ++ replaceWith a b ys
replaceWith a b (x:xs) = x : replaceWith a b xs

compress :: [Instr] -> ([Instr], [[Instr]])
compress = go [] [] where
    go compressed functions [] = (reverse compressed, reverse functions)
    go compressed functions (Function i:xs) = go (Function i : compressed) functions xs
    go compressed functions xs = go compressed functions' ys where
        bestPrefix = last 
                . takeWhile (\p -> count (isPrefixOf p) (tails xs) >= 3 && length (routineToString p) <= 20)
                $ inits xs
        ys = replaceWith bestPrefix [Function (length functions)] xs
        functions' = bestPrefix : functions

routineToString :: [Instr] -> String
routineToString = intercalate "," . map instrToString where
    instrToString (Move turn nbSteps) = (if turn == TurnLeft then "L" else "R") ++ "," ++ show nbSteps
    instrToString (Function n) = [chr (ord 'A' + n)]

codeToString :: ([Instr], [[Instr]]) -> String
codeToString (main, fs)= unlines $ map routineToString (main : fs) ++ ["n"]

solve' :: [Int] -> Maybe (Int, Int)
solve' pgm = do
    let grid = filter (/= "") . lines . map chr $ runProgram [] pgm
    let tiles = [pos | (pos, '#') <- flattenWithIndex' grid]
    start <- headMaybe [pos | (pos, '^') <- flattenWithIndex' grid]
    let grid' = fromLists' @U Seq grid
    let p1 = sum [x * y | p@(V2 y x) <- tiles, all (\p' -> (grid' !? toIx2 p') == Just '#') (adjacent p)]
    let instrs = map ord . codeToString . compress $ getWalk grid' start
    let pgm' = 2 : drop 1 pgm
    let p2 = last $ runProgram instrs pgm'
    pure (p1, p2)

solve :: Text -> IO ()
solve = aoc_ parser solve'