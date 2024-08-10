-- https://adventofcode.com/2018/day/13
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module Day13 (solve) where
import           AOC.Prelude hiding (head)
import           AOC (aoc')
import           AOC.Parser (Parser, choice, char, sepEndBy1, eol, some)
import           AOC.List (flattenWithIndex')
import           AOC.V2 (V2(..), west, east, north, south, turnLeft, turnRight, toIx2)
import           Data.Massiv.Array (Matrix, (!), U, Comp(Seq))
import qualified Data.Massiv.Array as A

type Grid = Matrix U Char
data Turn = TurnLeft | Straight | TurnRight
data Cart = Cart { position :: !(V2 Int), direction :: !(V2 Int), turn :: Turn }
type Input = (Grid, [Cart])

parser :: Parser [[Char]]
parser = some tile `sepEndBy1` eol where
    tile = choice (map char " /\\|-+^v<>")

rawToInput ::  [[Char]] -> Input
rawToInput grid = (A.fromLists' Seq grid, carts) where
    carts = flattenWithIndex' grid & mapMaybe \(pos, c) -> case c of
        '<' -> Just $ Cart pos west TurnLeft
        '>' -> Just $ Cart pos east TurnLeft
        '^' -> Just $ Cart pos north TurnLeft
        'v' -> Just $ Cart pos south TurnLeft
        _ -> Nothing

nextTurn :: Turn -> Turn
nextTurn TurnLeft = Straight
nextTurn Straight = TurnRight
nextTurn TurnRight = TurnLeft

moveCart :: Grid -> Cart -> Cart
moveCart grid (Cart pos dir turn) = case grid ! toIx2 pos' of
        '/'  | dir == north || dir == south -> Cart pos' (turnRight dir) turn
             | otherwise                    -> Cart pos' (turnLeft dir) turn
        '\\' | dir == north || dir == south -> Cart pos' (turnLeft dir) turn
             | otherwise                    -> Cart pos' (turnRight dir) turn
        '+'  -> let dir' = case turn of
                            TurnLeft -> turnLeft dir
                            Straight -> dir
                            TurnRight -> turnRight dir
               in Cart pos' dir' (nextTurn turn)
        _ -> Cart pos' dir turn
    where
    pos' = pos + dir

moveCarts1 :: Grid -> [Cart] -> Either (V2 Int) [Cart]
moveCarts1 grid = go [] where
    go done [] = Right done
    go done (cart:todo) =
        let cart' = moveCart grid cart in
        if hasCrashed cart' (done++todo)
            then Left cart'.position
            else go (cart':done) todo

moveCarts2 :: Grid -> [Cart] -> Either (V2 Int) [Cart]
moveCarts2 grid = go [] where
    go [] [] = error "moveCarts2: all cars have crashed"
    go [cart] [] = Left cart.position
    go done [] = Right done
    go done (cart:todo) =
        if hasCrashed cart' (done++todo)
            then go (filter f done) (filter f todo)
            else go (cart':done) todo
        where
        cart' = moveCart grid cart 
        f c = c.position /= cart'.position

hasCrashed :: Cart -> [Cart] -> Bool
hasCrashed cart = any (\c -> c.position == cart.position)

solveWith :: (Grid -> [Cart] -> Either (V2 Int) [Cart]) -> Input -> (Int, Int)
solveWith moveCarts (grid, initialCarts) = go initialCarts where
    go carts = case moveCarts grid (sortOn (\(Cart pos _ _) -> pos) carts) of
        Left (V2 y x) -> (x, y)
        Right carts' -> go carts'

solve :: Text -> IO ()
solve = aoc' parser (Just . rawToInput) (solveWith moveCarts1) (solveWith moveCarts2)