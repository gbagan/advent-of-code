-- https://adventofcode.com/2018/day/17
module Day17 (solve) where
import           AOC.Prelude hiding (toList)
import           Control.Monad.ST (ST, runST)
import           Data.List (maximum, minimum)
import           Data.Massiv.Array (MArray, Comp(Seq), U, Ix2(..), Sz(..), newMArray, read, readM, toList, write_)
import           Data.Massiv.Array.Unsafe (unsafeFreeze)
import           AOC (aoc_)
import           AOC.List (count)
import           AOC.Parser (Parser, sepEndBy1, eol, decimal, scanf)
import           AOC.V2(V2(..), down, left, right, toIx2, west, east)

data Vein = Vertical !Int !Int !Int | Horizontal !Int !Int !Int
type Grid s = MArray s U Ix2 Char

getXmax, getYmax, getYmin :: Vein -> Int
getXmax (Vertical x _ _) = x
getXmax (Horizontal _ _ x) = x
getYmax (Vertical _ _ y) = y
getYmax (Horizontal y _ _) = y
getYmin (Vertical _ y _) = y
getYmin (Horizontal y _ _) = y

parser :: Parser [Vein]
parser = (vertical <|> horizontal) `sepEndBy1` eol where
    vertical = [scanf|$Vertical x={decimal}, y={decimal}..{decimal}|]
    horizontal = [scanf|$Horizontal y={decimal}, x={decimal}..{decimal}|]

flow :: Grid s -> V2 Int -> ST s ()
flow grid pos = do
    let currentIx = toIx2 pos
    tile <- read grid currentIx
    unless (isNothing tile || tile == Just '~') do
        let downIx = toIx2 (down pos)

        when (tile == Just '.') do
            write_ grid currentIx '|'
            flow grid (down pos)
        
        downTile <- read grid downIx
        
        rightTile <- read grid $ toIx2 (right pos)
        when (downTile `elem` [Just '#', Just '~'] && rightTile == Just '.') do
            flow grid (right pos)
        
        leftTile <- read grid $ toIx2 (left pos)
        when (downTile `elem` [Just '#', Just '~'] && leftTile == Just '.') do
            flow grid (left pos)
        
        whenM (canFill grid pos) do
            fill grid pos

canFill :: Grid s -> V2 Int -> ST s Bool
canFill grid pos = canFill' grid pos west &&^ canFill' grid pos east

canFill' :: Grid s -> V2 Int -> V2 Int -> ST s Bool
canFill' grid pos dir = do
    tile <- read grid (toIx2 pos)
    case tile of
        Nothing -> pure False
        Just '.' -> pure False
        Just '#' -> pure True
        _ -> canFill' grid (pos + dir) dir

fill :: Grid s -> V2 Int -> ST s ()
fill grid pos = do
    fillAux grid pos west
    fillAux grid pos east

fillAux :: Grid s -> V2 Int -> V2 Int -> ST s ()
fillAux grid pos dir = do
    tile <- readM grid (toIx2 pos)
    unless (tile == '#') do
        write_ grid (toIx2 pos) '~'
        fillAux grid (pos + dir) dir

solve' :: [Vein] -> Maybe (Int, Int)
solve' veins = pure (p1, p2) where
    xmax = maximum $ map getXmax veins 
    ymax = maximum $ map getYmax veins    
    ymin = minimum $ map getYmin veins
    g = toList $ runST do
        grid <- newMArray @U (Sz2 (ymax+1-ymin) (xmax+1)) '.'
        for_ veins \case
            Vertical x y1 y2 ->
                for_ [y1..y2] \y -> write_ grid (Ix2 (y-ymin) x) '#'
            Horizontal y x1 x2 ->
                for_ [x1..x2] \x -> write_ grid (Ix2 (y-ymin) x) '#'
        flow grid (V2 0 500)
        unsafeFreeze Seq grid
    p1 = count (`elem` ['|', '~']) g
    p2 = count (== '~') g

solve :: Text -> IO ()
solve = aoc_ parser solve'