module Day19 (solve) where
import           RIO hiding (some)
import           RIO.List (scanl, transpose)
import           RIO.List.Partial (maximum)
import           Data.Either.Combinators (maybeToRight)
import qualified Data.Map as Map
import           Linear.V3 (V3(..))
import           Text.Megaparsec (sepEndBy1, some)
import qualified Text.Megaparsec.Char as P
import           Util (Parser, aoc', freqs, signedInteger)

type Scan = [V3 Int]

parser :: Parser [Scan]
parser = scan `sepEndBy1` P.eol where
    scan = P.string "--- scanner " *> some P.numberChar  *> P.string " ---" *> P.eol *> (coords `sepEndBy1` P.eol)
    coords = V3 <$> signedInteger <* P.char ',' <*> signedInteger <* P.char ',' <*> signedInteger

rotations :: V3 Int -> [V3 Int]
rotations p = scanl (&) p [r,t,t,t,r,c,c,c,r,t,t,t,r,c,c,c,r,t,t,t,r,c,c,c]
    where r (V3 x y z) = V3 x z (-y)
          t (V3 x y z) = V3 (-y) x z
          c (V3 x y z) = V3 y (-x) z

scanRotations :: Scan -> [Scan]
scanRotations = transpose . map rotations

alignAll :: [Scan] -> Maybe [(Scan, V3 Int)]
alignAll []            = Just []
alignAll (scan0:scans) = go [(scan0, V3 0 0 0)] [scan0] scans where
    go absolute _ [] = Just absolute
    go absolute (ref:refs) relative = go (found ++ absolute) (map fst found ++ refs) notFound
        where (notFound, found) = partitionEithers [maybeToRight scan $ align ref scan | scan <- relative]
    go _ _ _ = Nothing

align :: Scan -> Scan -> Maybe (Scan, V3 Int)
align scan scan' = listToMaybe [(map (+ pos) rot, pos) | rot <- scanRotations scan', pos <- overlap scan rot]

overlap :: Scan -> Scan -> [V3 Int]
overlap scan scan' = Map.keys . Map.filter (>= 12) . freqs $ (-) <$> scan <*> scan'

part1 :: [(Scan, V3 Int)] -> Int
part1 = length . nubOrd . concatMap fst

part2 :: [(Scan, V3 Int)] -> Int
part2 scans = maximum [dist x y | x <- positions, y <- positions] where
    positions = map snd scans
    dist v1 v2 = sum . abs $ v1 - v2

solve :: MonadIO m => Text -> m ()
solve = aoc' parser alignAll part1 part2