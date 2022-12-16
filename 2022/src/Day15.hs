-- https://adventofcode.com/2022/day/15
module Day15 (solve) where
import           RIO
import           RIO.List (find)
import           RIO.List.Partial (head)
import           Text.Megaparsec (sepEndBy1)
import           Text.Megaparsec.Char (eol, string)
import           Util (Parser, aoc, signedDecimal)

data Coords = Coords !Int !Int deriving (Eq, Show)
data Interval = Interval !Int !Int deriving (Eq)
data Scan = Scan !Coords !Coords !Int

parser :: Parser [Scan]
parser = line `sepEndBy1` eol where
    line = do
        _ <- string "Sensor at x="
        x1 <- signedDecimal
        _ <- string ", y="
        y1 <- signedDecimal
        _ <- string ": closest beacon is at x="
        x2 <- signedDecimal
        _ <- string ", y="
        y2 <- signedDecimal
        let sensor = Coords x1 y1
        let beacon = Coords x2 y2
        pure $ Scan sensor beacon (manhattan sensor beacon)

-- | manhattan distance
manhattan :: Coords -> Coords -> Int
manhattan (Coords x1 y1) (Coords x2 y2) = abs (x1 - x2) + abs (y1 - y2)

-- | two (discrete) intervals quasioverlap if their union is an interval
quasiOverlap :: Interval -> Interval -> Bool
quasiOverlap (Interval x1 y1) (Interval x2 y2) = max x1 x2 <= min y1 y2 + 1

-- | return the union of the two intervals if this union is an interval
-- | undefined otherwise
union :: Interval -> Interval -> Interval
union itv1@(Interval x1 y1) itv2@(Interval x2 y2)
    | quasiOverlap itv1 itv2 = Interval (min x1 x2) (max y1 y2)
    | otherwise = undefined

-- | return a set of disjoint intervals that contains the same points as the input
toDisjointUnion :: [Interval] -> [Interval]
toDisjointUnion [] = []
toDisjointUnion (itv:itvs) = case find (quasiOverlap itv) itvs of
    Nothing -> itv : toDisjointUnion itvs
    Just itv' -> toDisjointUnion $ union itv itv' : filter (/= itv') itvs

-- | interval between a ball (w.r.t. Manhattan distance) and a row
intersectionBallWithRow :: Coords -> Int -> Int -> Maybe Interval
intersectionBallWithRow (Coords cx cy) radius row
    | dx < 0 = Nothing
    | otherwise = Just $ Interval (cx - dx) (cx + dx)
    where dx = radius - abs (cy - row)

-- | number of points on the interval
itvLength :: Interval -> Int
itvLength (Interval a b) = b - a + 1

-- | union of disjoint intervals that does not cointain non detected beacons
intervalsWithoutBeacons :: Int -> [Scan] -> [Interval]
intervalsWithoutBeacons y =
        toDisjointUnion
        . mapMaybe (\(Scan sensor _ dist) -> intersectionBallWithRow sensor dist y)

part1 :: [Scan] -> Int
part1 pairs = nbBeacons - nbDetectedBeacons where
    nbBeacons = sum . map itvLength $ intervalsWithoutBeacons yTarget pairs
    nbDetectedBeacons = length $ nubOrd [x | Scan _ (Coords x y) _ <- pairs, y == yTarget]
    yTarget = 2000000

corners :: Scan -> [Coords]
corners (Scan (Coords x y) _ dist) = [l, u, r, d] where
    l = Coords (x-dist-1) y
    r = Coords (x+dist+1) y
    u = Coords x (y-dist-1)
    d = Coords x (y+dist+1)

{-
x1 + q = x2 + r
y1 + q = y2 - r
=> x1 + y1 + 2q = x2 + y2 
=> q = (x2 + y2 - x1 - y1) / 2
-}

diagonalIntersection :: Coords -> Coords -> [Coords]
diagonalIntersection (Coords x1 y1) (Coords x2 y2) =
    if x1 <= x2
    then [Coords (x1+q) (y1+q), Coords (x2-q) (y2-q)]
    else []
    where
    q = (x2 + y2 - x1 - y1) `div` 2

isNotDetected :: [Scan] -> Coords -> Bool
isNotDetected scans point =
    scans & all \(Scan sensor beacon dist) ->
                    point /= beacon && manhattan sensor point >= dist

part2 :: [Scan] -> Maybe Int
part2 scans = do
    Coords x y <- find (isNotDetected scans) candidates 
    pure $ x * 4000000 + y
    where
    vs = scans >>= corners
    candidates = concat (diagonalIntersection <$> vs <*> vs)
                        & filter \(Coords x y) -> x >= 0 && x < 4000000 
                                                && y >= 0 && y < 4000000

solve :: (HasLogFunc env) => Text -> RIO env ()
solve = aoc parser part1 part2