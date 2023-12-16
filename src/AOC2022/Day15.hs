-- https://adventofcode.com/2022/day/15
module AOC2022.Day15 (solve) where
import           Relude
import           AOC (aoc)
import           AOC.V2 (V2(..), manhattan)
import           AOC.Parser (Parser, eol, sepEndBy1, signedDecimal)
import           AOC.Interval (Interval(..), toDisjointUnion')

type Coords = V2 Integer
data Scan = Scan !Coords !Coords !Integer

parser :: Parser [Scan]
parser = line `sepEndBy1` eol where
    line = do
        x1 <- "Sensor at x=" *> signedDecimal
        y1 <- ", y=" *> signedDecimal
        x2 <- ": closest beacon is at x=" *> signedDecimal
        y2 <- ", y=" *> signedDecimal
        let sensor = V2 x1 y1
        let beacon = V2 x2 y2
        pure $ Scan sensor beacon (manhattan sensor beacon)

-- | interval between a ball (w.r.t. Manhattan distance) and a row
intersectionBallWithRow :: Coords -> Integer -> Integer -> Maybe (Interval Integer)
intersectionBallWithRow (V2 cx cy) radius row
    | dx < 0 = Nothing
    | otherwise = Just $ Interval (cx - dx) (cx + dx)
    where dx = radius - abs (cy - row)

-- | number of points on the interval
itvLength :: Interval Integer -> Integer
itvLength (Interval a b) = b - a + 1

-- | union of disjoint intervals that does not cointain non detected beacons
intervalsWithoutBeacons :: Integer -> [Scan] -> [Interval Integer]
intervalsWithoutBeacons y =
        toDisjointUnion'
        . mapMaybe (\(Scan sensor _ dist) -> intersectionBallWithRow sensor dist y)

part1 :: [Scan] -> Integer
part1 pairs = nbBeacons - nbDetectedBeacons where
    nbBeacons = sum . map itvLength $ intervalsWithoutBeacons yTarget pairs
    nbDetectedBeacons = genericLength $ ordNub [x | Scan _ (V2 x y) _ <- pairs, y == yTarget]
    yTarget = 2000000

corners :: Scan -> [Coords]
corners (Scan (V2 x y) _ dist) = [l, u, r, d] where
    l = V2 (x-dist-1) y
    r = V2 (x+dist+1) y
    u = V2 x (y-dist-1)
    d = V2 x (y+dist+1)

{-
x1 + q = x2 + r
y1 + q = y2 - r
=> x1 + y1 + 2q = x2 + y2 
=> q = (x2 + y2 - x1 - y1) / 2
-}

diagonalIntersection :: Coords -> Coords -> [Coords]
diagonalIntersection (V2 x1 y1) (V2 x2 y2) =
    if x1 <= x2
    then [V2 (x1+q) (y1+q), V2 (x2-q) (y2-q)]
    else []
    where
    q = (x2 + y2 - x1 - y1) `div` 2

isNotDetected :: [Scan] -> Coords -> Bool
isNotDetected scans point =
    scans & all \(Scan sensor beacon dist) ->
                    point /= beacon && manhattan sensor point >= dist

part2 :: [Scan] -> Maybe Integer
part2 scans = do
    V2 x y <- find (isNotDetected scans) candidates 
    pure $ x * 4000000 + y
    where
    vs = scans >>= corners
    candidates = concat (diagonalIntersection <$> vs <*> vs)
                        & filter \(V2 x y) -> x >= 0 && x < 4000000 
                                            && y >= 0 && y < 4000000

solve :: Text -> IO ()
solve = aoc parser part1 part2