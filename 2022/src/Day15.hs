-- https://adventofcode.com/2022/day/1
module Day15 (solve) where
import           RIO
import           RIO.List (find)
import           RIO.List.Partial (head)
import           Text.Megaparsec (sepEndBy1)
import           Text.Megaparsec.Char (eol, string)
import           Linear.V2 (V2(..))
import           Util (Parser, aoc, signedDecimal)

type Position = V2 Int
data Interval = Interval !Int !Int deriving (Eq)

parser :: Parser [(Position, Position)]
parser = line `sepEndBy1` eol where
    line = do 
        _ <- string "Sensor at x="
        x1 <- signedDecimal
        _ <- string ", y="
        y1 <- signedDecimal
        _ <- string": closest beacon is at x="
        x2 <- signedDecimal
        _ <- string ", y=" 
        y2 <- signedDecimal
        pure (V2 x1 y1, V2 x2 y2)

-- | manhattan distance
manhattan :: Position -> Position -> Int
manhattan (V2 x1 y1) (V2 x2 y2) = abs (x1 - x2) + abs (y1 - y2)

-- | two (discrete) intervals quasioverlap if their union is an interval
quasiOverlap :: Interval -> Interval -> Bool
quasiOverlap (Interval x1 y1) (Interval x2 y2) = max x1 x2 <= min y1 y2 + 1

-- | return the union of the two intervals if this union is an interval
-- | undefined otherwise
union :: Interval -> Interval -> Interval
union itv1@(Interval x1 y1) itv2@(Interval x2 y2)
    | quasiOverlap itv1 itv2 = Interval (min x1 x2) (max y1 y2)
    | otherwise = undefined

-- | return a set disjoint intervals that contains the same points as the input
toDisjointUnion :: [Interval] -> [Interval]
toDisjointUnion [] = []
toDisjointUnion (itv:itvs) = case find (quasiOverlap itv) itvs of
    Nothing -> itv : toDisjointUnion itvs
    Just itv' -> toDisjointUnion $ union itv itv' : filter (/= itv') itvs

-- | interval between a circle (w.r.t. Manhattan distance) and a row
intersectionCircleWithRow :: Position -> Int -> Int -> Maybe Interval
intersectionCircleWithRow (V2 cx cy) radius row
    | dx < 0 = Nothing
    | otherwise = Just $ Interval (cx - dx) (cx + dx)
    where dx = radius - abs (cy - row)

-- | number of points on the interval
itvLength :: Interval -> Int
itvLength (Interval a b) = b - a + 1

-- | union of disjoint intervals that does not cointain non detected beacons
intervalsWithoutBeacons :: Int -> [(Position, Position)] -> [Interval]
intervalsWithoutBeacons y =
        toDisjointUnion
        . mapMaybe (\(scanner, beacon) ->
            let dist = manhattan scanner beacon
            in intersectionCircleWithRow scanner dist y
        )

part1 :: [(Position, Position)] -> Int
part1 pairs = nbBeacons - nbDetectedBeacons where
    nbBeacons = sum . map itvLength $ intervalsWithoutBeacons yTarget pairs
    nbDetectedBeacons = length $ nubOrd [x | (_, V2 x y) <- pairs, y == yTarget]
    yTarget = 2000000

part2 :: [(Position, Position)] -> Maybe Int
part2 pairs = sol where 
    itvsPerRow = map (\y -> (y, intervalsWithoutBeacons y pairs)) [0..4000000]
    sol = find (\(_, itv) -> length itv >=2) itvsPerRow
               <&> \(j, intvs) -> let Interval _ b = head intvs in 4000000 * (b + 1) + j

solve :: (HasLogFunc env) => Text -> RIO env ()
solve = aoc parser part1 part2