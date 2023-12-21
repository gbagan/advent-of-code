-- https://adventofcode.com/2022/day/16
module Day16 (solve) where
import           AOC.Prelude hiding (get, init)
import           Data.List (maximum)
import qualified Data.IntSet as IS
import qualified Data.Vector as V
import           Data.Vector ((!))
import qualified Data.HashMap.Strict as M
import           AOC (aoc)
import           AOC.Parser (Parser, sepBy1, sepEndBy1, some, eol, upperChar, decimal)
import           AOC.List (maximumDef)

type Duration = Int
data Input = Input !Graph !DistanceMat !Int
data Valve = Valve !Int ![Int] -- flow rate and neighbors
type Graph = Vector Valve
type DistanceMat = HashMap (Int, Int) Int

parser :: Parser Input
parser = mapToInput . M.fromList <$> valve `sepEndBy1` eol where
    valve = do
        name <- "Valve " *> some upperChar
        rate <- " has flow rate=" *> decimal
        _ <- "; tunnels lead to valves " <|> "; tunnel leads to valve "
        nbors <- some upperChar `sepBy1` ", "
        pure (name, (rate, nbors))

mapToInput :: HashMap String (Int, [String]) -> Input
mapToInput m = Input graph dist init where
    graph = V.generate (V.length vkeys) \i ->
        let (flow, nbors) = m M.! (vkeys ! i) in
        Valve flow ((reverseMap M.!) <$> nbors)
    keys = M.keys m
    vkeys = V.fromList keys :: Vector String
    reverseMap = M.fromList $ zip keys [0..]
    dist = distance graph
    init = reverseMap M.! "AA"

usefulValves :: Input -> [Int]
usefulValves (Input graph _ _) = [name | (name, Valve flow _) <- zip [0..] (V.toList graph), flow > 0]

distance :: Graph -> DistanceMat
distance graph = foldl' go initdistance $ (,,) <$> keys <*> keys <*> keys where 
    keys = [0 .. V.length graph - 1]
    initdistance = M.fromList [((v1, v2), 1) | (v1, Valve _ nbors) <- zip [0..] (V.toList graph), v2 <- nbors]
    go dist (k, i, j) = M.alter (Just . min (get dist (i, k) + get dist (k, j)) . fromMaybe 10000) (i, j) dist
    get dist (i, j) | i == j = 0
                    | otherwise = fromMaybe 10000 (M.lookup (i, j) dist)

travel :: Input -> Duration -> IntSet -> Int
travel (Input graph dist init) = go init where
    go current duration remaining =
        maximumDef 0 [ duration' * flow + go nextv duration' (IS.delete nextv remaining)
                     | nextv <- IS.toList remaining
                     , let Valve flow _ = graph ! nextv
                     , let duration' = duration - dist M.! (current, nextv) - 1
                     , duration' >= 0
                     ]

part1 :: Input -> Int
part1 input = travel input 30 valves where
    valves = IS.fromList $ usefulValves input

splits :: [a] -> [([a], [a])]
splits [] = [([], [])]
splits [x] = [([x], [])]
splits (x:xs) = splits xs >>= \(l1, l2) -> [(x:l1, l2), (l1, x:l2)]

part2 :: Input -> Int
part2 input = maximum [ travel input 26 (IS.fromList valves1) 
                      + travel input 26 (IS.fromList valves2)
                      | (valves1, valves2) <- splits (usefulValves input)
                      ]

solve :: Text -> IO ()
solve = aoc parser part1 part2