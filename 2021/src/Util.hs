module Util where
import           RIO
import           RIO.List (group, sort, genericLength)
import           RIO.List.Partial (head, (!!))
import qualified RIO.Set as Set
import qualified RIO.Text as Text
import           Text.Megaparsec (Parsec, parse, errorBundlePretty)
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Data.Map as Map

type Parser = Parsec Void Text
type BinParser = Parsec Void [Bool]
type Point = (Int, Int)

aocTemplate :: (MonadReader env m, MonadIO m, HasLogFunc env) => Parser a -> (a -> Maybe b) -> (b -> Maybe Int) -> (b -> Maybe Int) -> Text -> m ()
aocTemplate parser precomp part1 part2 s = do
    case parse parser "" s of
        Left err -> logInfo $ display $ Text.pack $ errorBundlePretty err
        Right input -> do
            case precomp input of
                Nothing -> logInfo "precomputing failed"
                Just p -> do
                    case part1 p of
                        Nothing -> logInfo "  part 1: no solution found"
                        Just n ->  logInfo $ "  part 1: " <> display n
                    case part2 p of
                        Nothing -> logInfo "  part 2: no solution found"
                        Just n ->  logInfo $ "  part 2: " <> display n

adjacentPoints :: Point -> [Point]
adjacentPoints (x, y) = [(x-1, y), (x+1, y), (x, y-1), (x, y+1)]

kingAdjacentPoints :: Point -> [Point]
kingAdjacentPoints (x, y) = adjacentPoints (x, y) ++ [(x-1, y-1), (x+1, y-1), (x-1, y+1), (x+1, y+1)]

range :: Int -> Int -> [Int]
range x y = if x < y then [x .. y] else [y .. x]

count :: (a -> Bool) -> [a] -> Int
count f = length . filter f

freqs :: Ord a => [a] -> Map a Int
freqs = Map.fromListWith (+) . map (,1)

sortNub :: Ord a => [a] -> [a]
sortNub = map head . group . sort

average :: [Int] -> Double
average xs = realToFrac (sum xs) / genericLength xs

majority :: (a -> Bool) -> [a] -> Bool
majority f l = 2* count f l >= length l

median :: Ord a => [a] -> a
median l = sort l !! (length l `div` 2)

listTo2dMap :: [[a]] -> Map Point a
listTo2dMap l = 
    Map.fromList
        [((i, j), v) 
        | (j, row) <- zip [0..] l
        , (i, v) <- zip [0..] row
        ]

binToInt :: [Bool] -> Int
binToInt = foldl' (\acc x -> acc * 2 + fromEnum x) 0

cartesianProduct :: [a] -> [b] -> [(a, b)]
cartesianProduct l1 l2 = (,) <$> l1 <*> l2

signedInteger :: Parser Int
signedInteger = L.decimal <|> P.char '-' *> (negate <$> L.decimal)

bitP :: Parser Bool
bitP = False <$ P.char '0' <|> True <$ P.char '1'

dijkstra :: (Ord v, Real w) => (v -> [(v, w)]) -> v -> v -> Maybe w
dijkstra nbors source target = go Set.empty (Set.singleton (0, source)) where
    go visited queue = case Set.minView queue of
        Nothing -> Nothing
        Just ((cost, v), queue')
            | v == target          -> Just cost
            | Set.member v visited -> go visited queue'
            | otherwise            -> go
                                        (Set.insert v visited)
                                        (foldr Set.insert queue' [(w+cost, u) | (u, w) <- nbors v])