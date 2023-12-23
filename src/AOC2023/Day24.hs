-- https://adventofcode.com/2023/day/24
module Day24 (solve) where
import           AOC.Prelude
import           AOC (aoc)
import           AOC.Parser (Parser, sepEndBy1, eol, hspace, signedDecimal)
import           AOC.List (count, pairwise)
import           AOC.V2 (V2(..))
import           AOC.V3 (V3(..))
import           Z3.Monad
import           System.IO.Unsafe (unsafePerformIO)

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
    q1 = fromIntegral $ (px2 * py4 - py2 * px4) * vx1 - (px1 * py3 - py1 * px3) * vx2
    q2 = fromIntegral $ (px2 * py4 - py2 * px4) * vy1 - (px1 * py3 - py1 * px3) * vy2
    d = fromIntegral $ vx1 * vy2 - vy1 * vx2
    x = q1 / d
    y = q2 / d

crossesInsideTestArea :: Int -> Int -> Hailstone -> Hailstone -> Bool
crossesInsideTestArea  start end h1@(Hailstone (V3 px1 _ _) (V3 vx1 _ _)) 
                                 h2@(Hailstone (V3 px2 _ _) (V3 vx2 _ _)) =
    fromMaybe False do
        V2 x y <- cross h1 h2
        guard $ not (vx1 < 0 && x > fromIntegral px1 || vx1 > 0 && x < fromIntegral px1)
        guard $ not (vx2 < 0 && x > fromIntegral px2 || vx2 > 0 && x < fromIntegral px2)
        let start' = fromIntegral start
        let end' = fromIntegral end
        guard $ x >= start' && y >= start' && x <= end' && y <= end'
        pure True

part1 :: [Hailstone] -> Int
part1 = count id . pairwise (crossesInsideTestArea 200_000_000_000_000 400_000_000_000_000)

script :: [Hailstone] -> Z3 (Maybe [Integer])
script hailstones = do
    _0 <- mkRealNum (0 :: Int)
    px <- mkFreshRealVar "px"
    py <- mkFreshRealVar "py"
    pz <- mkFreshRealVar "pz"
    vx <- mkFreshRealVar "vy"
    vy <- mkFreshRealVar "vy"
    vz <- mkFreshRealVar "vz"
    forM_ (zip [(0::Int)..] hailstones) \(i, Hailstone (V3 pxi pyi pzi) (V3 vxi vyi vzi)) -> do
        ti <- mkFreshRealVar ("t" <> show i)
        _vxi <- mkRealNum (-vxi)
        _vyi <- mkRealNum (-vyi)
        _vzi <- mkRealNum (-vzi)
        s1 <- mkAdd =<< sequence [pure px, mkMul [ti, vx], mkRealNum (-pxi), mkMul [ti, _vxi]]
        s2 <- mkAdd =<< sequence [pure py, mkMul [ti, vy], mkRealNum (-pyi), mkMul [ti, _vyi]]
        s3 <- mkAdd =<< sequence [pure pz, mkMul [ti, vz], mkRealNum (-pzi), mkMul [ti, _vzi]]
        assert =<< mkEq s1 _0
        assert =<< mkEq s2 _0
        assert =<< mkEq s3 _0
    fmap snd $ withModel $ \m ->
        catMaybes <$> mapM (evalInt m) [px,py,pz]

part2 :: [Hailstone] -> Maybe Integer
part2 hailstones =
    unsafePerformIO do
        sol <- evalZ3 (script hailstones)
        pure $ sum <$> sol

solve :: Text -> IO ()
solve = aoc parser part1 part2