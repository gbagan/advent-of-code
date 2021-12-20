module Day19 (solve) where
import           Data.Either (partitionEithers)
import           Data.Either.Combinators (maybeToRight)
import           Data.Function ((&))
import           Data.List (transpose)
import           Data.Maybe (listToMaybe)
import qualified Data.Map as Map
import           Text.Megaparsec (sepEndBy1, some)
import qualified Text.Megaparsec.Char as P
import           Util (Parser, aocTemplate, freqs, signedInteger, sortNub)

type Point = (Int, Int, Int)
type Scan = [Point]

parser :: Parser [Scan]
parser = scan `sepEndBy1` P.eol where
    scan = P.string "--- scanner " *> some P.numberChar  *> P.string " ---" *> P.eol *> (coords `sepEndBy1` P.eol)
    coords = (,,) <$> signedInteger <* P.char ',' <*> signedInteger <* P.char ',' <*> signedInteger

add :: Point -> Point -> Point
add (x, y, z) (x', y', z') = (x + x', y + y', z + z')
sub :: Point -> Point -> Point
sub (x, y, z) (x', y', z') = (x - x', y - y', z - z')

rotations :: Point -> [Point]
rotations p = scanl (&) p [r,t,t,t,r,t,t,t,r,t,t,t, r.t.r.r, t,t,t,r,t,t,t,r,t,t,t]
    where r (x,y,z) = (x,z,-y)
          t (x,y,z) = (-y,x,z)

scanRotations :: Scan -> [Scan]
scanRotations = transpose . map rotations

alignAll :: [Scan] -> [(Scan, Point)]
alignAll []            = []
alignAll (scan0:scans) = go [(scan0, (0, 0, 0))] [scan0] scans where
    go absolute _ [] = absolute
    go absolute (ref:refs) relative = go (found ++ absolute) (map fst found ++ refs) notFound
        where (notFound, found) = partitionEithers [maybeToRight scan $ align ref scan | scan <- relative]
    go _ _ _ = []

align :: Scan -> Scan -> Maybe (Scan, Point)
align scan scan' = listToMaybe [(map (add pos) rot, pos) | rot <- scanRotations scan', pos <- overlap scan rot]

overlap :: Scan -> Scan -> [Point]
overlap scan scan' = Map.keys . Map.filter (>= 12) . freqs $ sub <$> scan <*> scan'

part1 :: [(Scan, Point)] -> Int
part1 = length . sortNub . concatMap fst

part2 :: [(Scan, Point)] -> Int
part2 scans = maximum [dist x y | x <- positions, y <- positions] where
    positions = map snd scans
    dist (x1, y1, z1) (x2, y2, z2) = abs(x1-x2) + abs(y1-y2) + abs(z1-z2)

solve :: String -> IO ()
solve = aocTemplate parser (pure . alignAll) (pure . part1) (pure . part2)