-- https://adventofcode.com/2023/day/24
module Day24 (solve) where
import           AOC.Prelude hiding (init, last)
import           Data.List (init, last)
import           AOC (aoc)
import           AOC.Parser (Parser, sepEndBy1, eol, hspace, signedDecimal)
import           AOC.List (count, pairwise)
import           AOC.Number (toFrac)
import           AOC.V2 (V2(..))
import           AOC.V3 (V3(..))
import           AOC.LinearAlgebra (solveLinearSystem)

type Position = V3 Integer
type Velocity = V3 Integer
data Hailstone = Hailstone !Position !Velocity

parser :: Parser [Hailstone]
parser = hailstone `sepEndBy1` eol where
    hailstone = Hailstone <$> v3 <* " @ " <*> v3
    v3 = do 
        x <- hspace *> signedDecimal 
        y <- ", " *> hspace *> signedDecimal 
        z <- "," *> hspace *> signedDecimal
        pure $ V3 x y z

cross :: Hailstone -> Hailstone -> Maybe (V2 Rational)
cross (Hailstone (V3 px1 py1 _) (V3 vx1 vy1 _)) (Hailstone (V3 px2 py2 _) (V3 vx2 vy2 _)) =
    if d == 0
        then Nothing
        else Just $! V2 x y
    where
    px3 = px1 + vx1
    px4 = px2 + vx2
    py3 = py1 + vy1
    py4 = py2 + vy2
    q1 = toFrac $ (px2 * py4 - py2 * px4) * vx1 - (px1 * py3 - py1 * px3) * vx2
    q2 = toFrac $ (px2 * py4 - py2 * px4) * vy1 - (px1 * py3 - py1 * px3) * vy2
    d = toFrac $ vx1 * vy2 - vy1 * vx2
    x = q1 / d
    y = q2 / d

crossesInsideTestArea :: Int -> Int -> Hailstone -> Hailstone -> Bool
crossesInsideTestArea  start end h1@(Hailstone (V3 px1 py1 _) (V3 vx1 vy1 _)) 
                                 h2@(Hailstone (V3 px2 py2 _) (V3 vx2 vy2 _)) =
    fromMaybe False do
        V2 x y <- cross h1 h2
        guard $ toFrac vx1 * (x - toFrac px1) >= 0
        guard $ toFrac vx2 * (x - toFrac px2) >= 0
        guard $ toFrac vy1 * (x - toFrac py1) >= 0
        guard $ toFrac vy2 * (x - toFrac py2) >= 0
        let start' = toFrac start
        let end' = toFrac end
        pure $ x >= start' && y >= start' && x <= end' && y <= end'

part1 :: [Hailstone] -> Int
part1 = count id . pairwise (crossesInsideTestArea 200_000_000_000_000 400_000_000_000_000)

buildEquations :: Hailstone -> [[Rational]]
buildEquations hs = map (map toFrac)
    [ [vy, -vx, 0, -py, px, 0, px * vy - py * vx]
    , [vz, 0, -vx, -pz, 0, px, px * vz - pz * vx]
    , [0, vz, -vy, 0, -pz, py, py * vz - pz * vy]
    ] where
    Hailstone (V3 px py pz) (V3 vx vy vz) = hs

part2 :: [Hailstone] -> Maybe Integer
part2 (hs1 : hs2 :  hs3 : _) = do
    let eqs1 = buildEquations hs1
        eqs2 = buildEquations hs2
        eqs3 = buildEquations hs3
        (.-) = zipWith (zipWith (-))
        equations =  eqs1 .- eqs2 ++ eqs2 .- eqs3
        mat = map init equations
        vec = map last equations
    sol <- solveLinearSystem mat vec
    pure . floor . sum $ take 3 sol
part2 _ = error "cannot happen"

solve :: Text -> IO ()
solve = aoc parser part1 part2