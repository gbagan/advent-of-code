-- https://adventofcode.com/2022/day/14
module Day14 (solve) where
import           RIO
import           RIO.List.Partial (maximum)
import qualified RIO.HashSet as Set
import           Text.Megaparsec (sepEndBy1, sepBy1)
import           Text.Megaparsec.Char (char, eol, string)
import           Text.Megaparsec.Char.Lexer (decimal)
import           Util (Parser, aoc', cartesianProduct)
import           Data.HashTable.ST.Basic as H

type Scan = [(Int, Int)]
type Rocks = HashSet (Int, Int)

parser :: Parser [Scan]
parser = scan `sepEndBy1` eol where
    scan = position `sepBy1` string " -> "
    position = (,) <$> decimal <* char ',' <*> decimal

origin :: (Int, Int)
origin = (500, 0)

adj :: (Int, Int) -> [(Int, Int)]
adj (x, y) = [(x, y+1), (x-1, y+1), (x+1, y+1)]

drawLine :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
drawLine (x1, y1) (x2, y2) | x1 > x2 = drawLine (x2, y1) (x1, y2)
                           | y1 > y2 = drawLine (x1, y2) (x2, y1)
                           | otherwise = cartesianProduct [x1..x2] [y1..y2]

drawScan :: Scan -> [(Int, Int)]
drawScan [] = []
drawScan (s:ss) = concat $ zipWith drawLine (s:ss) ss

precomp :: [Scan] -> Rocks
precomp scans = Set.fromList $ scans >>= drawScan

-- | return the final position of a sand unit that falls from the origin
fall :: Rocks -> Int -> (Int, Int)
fall rocks bottom = go origin where
    go xy = case [xy' | xy' <- adj xy, not (xy' `Set.member` rocks) && snd xy' < bottom] of
            [] -> xy
            (xy':_) -> go xy'

-- | number of times one can fill a sand unit until the predicate is satisfied
fillUntil :: ((Int, Int) -> Bool) -> Rocks -> Int
fillUntil predicate rocks = go rocks 0 where
    go rocks' i = 
        let xy = fall rocks' bottom in
        if predicate xy
        then i
        else go (Set.insert xy rocks') (i + 1)
    bottom = 2 + maximum (map snd (Set.toList rocks))

part1 :: Rocks -> Int
part1 rocks = fillUntil ((== bottom) . snd) rocks  where
    bottom = 1 + maximum (map snd (Set.toList rocks))

part2 :: Rocks -> Int
part2 rocks = runST $ do
    sand <- H.newSized 30000
    forM_ [0..bottom] $ \j -> do
        forM_ [500-j..500+j] $ \i -> do
            let notRock = not $ (i, j) `Set.member` rocks
            v1 <- H.lookup sand (i-1, j-1)
            v2 <- H.lookup sand (i, j-1)
            v3 <- H.lookup sand (i+1, j-1)
            when (j == 0 || notRock && (isJust v1 || isJust v2 || isJust v3)) $
                H.insert sand (i, j) ()
    H.size sand
    where bottom = 1 + maximum (map snd (Set.toList rocks))

solve :: Text -> RIO env ()
solve = aoc' parser (pure . precomp) part1 part2


{- old implementation
part2 :: Rocks -> Int
part2 rocks = 1 + fillUntil (== origin) rocks
-}
