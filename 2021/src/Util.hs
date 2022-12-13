module Util where
import           RIO
import           RIO.List (sort, genericLength)
import           RIO.List.Partial ((!!))
import qualified RIO.Set as Set
import qualified RIO.Text as Text
import qualified RIO.Vector as V
import           RIO.Vector ((!?))
import           RIO.Vector.Partial ((!))
import           Text.Megaparsec (Parsec, parse, errorBundlePretty)
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Data.Map as Map

type Parser = Parsec Void Text
type BinParser = Parsec Void [Bool]
type Point = (Int, Int)

-- templates

aoc :: (Show b, Show c, HasLogFunc env) =>
        Parser a -> (a -> b) -> (a -> c) -> Text -> RIO env ()
aoc parser = aoc' parser pure

aoc' :: (Show c, Show d, HasLogFunc env) =>
        Parser a -> (a -> Maybe b) -> (b -> c) -> (b -> d) -> Text -> RIO env ()
aoc' parser precomp part1 part2 input = do
    case parse parser "" input of
        Left err -> logInfo $ display $ Text.pack $ errorBundlePretty err
        Right parsed ->
            case precomp parsed of
                Nothing -> logInfo "  precomputation has failed"
                Just p -> do
                    logInfo $ "  part 1: " <> displayShow (part1 p)
                    logInfo $ "  part 2: " <> displayShow (part2 p)

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

-- matrices i.e. two dimensional arrays

type Matrix a = Vector (Vector a)

(!!!) :: Matrix a -> (Int, Int) -> a
v !!! (i, j) = v ! i ! j

(!!?) :: Matrix a -> (Int, Int) -> Maybe a
v !!? (i, j) = v !? i >>= (!?j)

flatMat :: Matrix a -> [(Int, Int, a)]
flatMat = join . zipWith (\x -> zipWith (x,,) [0..]) [0..] . map V.toList . V.toList

matFromList :: [[a]] -> Matrix a
matFromList = V.fromList . map V.fromList

matGenerate :: Int -> Int -> (Int -> Int -> a) -> Matrix a
matGenerate n m f = V.generate n \i -> V.generate m \j -> f i j

binToInt :: [Bool] -> Int
binToInt = foldl' (\acc x -> acc * 2 + fromEnum x) 0

cartesianProduct :: [a] -> [b] -> [(a, b)]
cartesianProduct l1 l2 = (,) <$> l1 <*> l2

signedInteger :: Parser Int
signedInteger = L.decimal <|> P.char '-' *> (negate <$> L.decimal)

bitP :: Parser Bool
bitP = False <$ P.char '0' <|> True <$ P.char '1'