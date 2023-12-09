-- https://adventofcode.com/2022/day/17
module AOC2022.Day17 (solve) where
import           Relude hiding (some)
import           Data.List (maximum)
import qualified Data.HashSet as Set
import qualified Data.List.NonEmpty as NE
import           Linear.V2 (V2(..))
import           Data.Stream.Infinite (Stream(..))
import qualified Data.Stream.Infinite as Stream
import           Text.Megaparsec (some)
import           Text.Megaparsec.Char (char)
import           Util (Parser, aoc)

data Jet = L | R deriving (Eq)
type Rock = HashSet (V2 Int)
data Rest = Rest !Rock !Int

parser :: Parser [Jet]
parser = some direction where
    direction = L <$ char '<' <|> R <$ char '>'

makeIndexedStream :: [a] -> Stream (Int, a)
makeIndexedStream  = Stream.cycle . NE.fromList . zip [0..]

rockStream :: Stream (Int, Rock)
rockStream = makeIndexedStream
    [ Set.fromList [V2 0 0, V2 1 0, V2 2 0, V2 3 0]
    , Set.fromList [V2 1 1, V2 0 1, V2 1 0, V2 2 1, V2 1 2]
    , Set.fromList [V2 0 0, V2 1 0, V2 2 0, V2 2 1, V2 2 2]
    , Set.fromList [V2 0 0, V2 0 1, V2 0 2, V2 0 3]
    , Set.fromList [V2 0 0, V2 0 1, V2 1 0, V2 1 1]
    ]

initialRest :: Rest
initialRest = Rest (Set.fromList [V2 i (-1) | i <- [0..6]]) 0

findRepetition :: Hashable a => Stream a -> a
findRepetition = go Set.empty where
    go m (x :> xs) | x `Set.member` m = x
                   | otherwise  = go (Set.insert x m) xs

rockHeight :: Rock -> Int
rockHeight rock = 1 + maximum [y | V2 _ y <- Set.toList rock]

translateRock :: Int -> Int -> Rock -> Rock
translateRock dx dy = Set.map (+ V2 dx dy)

applyJet :: Jet -> Rock -> Rock -> Rock
applyJet jet rest rock
    | Set.null (rock' `Set.intersection` rest) 
        &&  all (\(V2 x _) -> 0 <= x && x <= 6) (Set.toList rock')
        = rock'
    | otherwise = rock
    where
    rock' = translateRock (if jet == L then -1 else 1) 0 rock 

putRock :: Stream (Int, Jet) -> Rock -> Rest -> (Rest, Stream (Int, Jet))
putRock ((_, jet):>jets') rock r@(Rest rest height) =
    let rock' = applyJet jet rest rock
        rock'' = translateRock 0 (-1) rock'
    in
    if Set.null (rock'' `Set.intersection` rest)
        then putRock jets' rock'' r
        else (Rest (Set.union rest rock') (max height (rockHeight rock')), jets')


simulate :: Rest -> Stream (Int, Rock) -> Stream (Int, Jet) -> Stream (Int, Int, Int, Rest)
simulate = go 0 where
    go idx rest@(Rest _ height) ((rockIdx, rock) :> rocks') jets@((jetIdx,_):> _) =
        (idx, rockIdx, jetIdx, rest) :> go (idx+1) rest' rocks' jets'
        where
        (rest', jets') = putRock jets rock' rest
        rock' = translateRock 2 (height+3) rock 

part1 :: [Jet] -> Int
part1 jets = height where
    (_, _, _, Rest _ height) :> _ = simulate initialRest rockStream jetStream
                                       & Stream.filter \(j, _, _, _) -> j == 2022
    jetStream = makeIndexedStream jets
    
part2 :: [Jet] -> Int
part2 jets = height * cycles + height' where
    states = simulate initialRest rockStream jetStream
    jetStream = makeIndexedStream jets
    (rockIdx, jetIdx) = findRepetition . fmap (\(_, x, y, _) -> (x, y)) $ states
    _ :> (idx1, _, _, rest1@(Rest _ height1)) :> (idx2, _, _, Rest _ height2) :> _ =
        Stream.filter (\(_, x, y, _) -> x == rockIdx && y == jetIdx) states
    height = height2 - height1
    period = idx2 - idx1
    (cycles, remaining) = (1000000000000 - idx1) `divMod` period
    (_, _, _, Rest _ height') :> _ = Stream.filter (\(idx, _, _, _) -> idx == remaining) $
                                    simulate
                                        rest1
                                        (Stream.drop rockIdx rockStream)
                                        (Stream.drop jetIdx jetStream)

solve :: MonadIO m => Text -> m ()
solve = aoc parser part1 part2