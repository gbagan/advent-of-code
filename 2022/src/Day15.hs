-- https://adventofcode.com/2022/day/15
module Day15 (solve) where
import           RIO
import           RIO.List (find)
import           RIO.List.Partial (head)
import           Text.Megaparsec (sepEndBy1)
import           Text.Megaparsec.Char (eol, string)
import           Util (Parser, aoc, cartesianProduct, signedDecimal)

data Coords = Coords !Int !Int deriving (Eq)
data Interval = Interval !Int !Int deriving (Eq)
data Scan = Scan !Coords !Coords

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
        Scan (Coords x1 y1) . Coords x2 <$> signedDecimal

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

-- | interval between a circle (w.r.t. Manhattan distance) and a row
intersectionCircleWithRow :: Coords -> Int -> Int -> Maybe Interval
intersectionCircleWithRow (Coords cx cy) radius row
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
        . mapMaybe (\(Scan scanner beacon) ->
            let dist = manhattan scanner beacon
            in intersectionCircleWithRow scanner dist y
        )

part1 :: [Scan] -> Int
part1 pairs = nbBeacons - nbDetectedBeacons where
    nbBeacons = sum . map itvLength $ intervalsWithoutBeacons yTarget pairs
    nbDetectedBeacons = length $ nubOrd [x | Scan _ (Coords x y) <- pairs, y == yTarget]
    yTarget = 2000000

part2 :: [Scan] -> Maybe Int
part2 pairs = sol where
    itvsPerRow = map (\y -> (y, intervalsWithoutBeacons y pairs)) [0..4000000]
    sol = find (\(_, itv) -> length itv >=2) itvsPerRow
               <&> \(j, intvs) -> let Interval _ b = head intvs in 4000000 * (b + 1) + j

{-
diagonals :: (Coords, Int) -> [(Coords, Coords)]
diagonals (Coords x y, dist) = [(l, d), (l, u), (r, d), (r, u)] where
    l = Coords (x-dist) y
    r = Coords (x+dist) y
    u = Coords x (y-dist)
    d = Coords x (y+dist)


diagonalIntersection :: Coords -> Coords -> [Coords]
diagonalIntersection (Coords x1 y1) (Coords x2 y2) =
    let q = y1 + y2 - x1 - x2
        r = x1 - x2 + y2 - y1 in
        if even q && even r
        then [Coords (x1+q`div`2) (y1+r`div`2), Coords (x2-q`div`2) (y2-q`div`2)]
        else []

-}

solve :: (HasLogFunc env) => Text -> RIO env ()
solve = aoc parser part1 part2