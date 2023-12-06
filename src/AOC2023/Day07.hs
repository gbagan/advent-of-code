-- https://adventofcode.com/2023/day/7
module AOC2023.Day07 (solve) where
import           RIO hiding (some)
import           RIO.List (sort, sortBy)
import qualified RIO.Map as Map
import           Text.Megaparsec (sepEndBy1, some)
import           Text.Megaparsec.Char (eol, hspace)
import           Text.Megaparsec.Char.Lexer (decimal)
import           Util (Parser, aoc, freqs')

type Bid = ([Card], Int)
type Input = [Bid]
data Card = C2 | C3 | C4 | C5 |  C6 | C7 | C8 | C9 | T | J | Q | K | A deriving (Eq, Ord, Show)
data HandType = HighCard | OnePair | TwoPair | Three |  Full | Four | Five deriving (Eq, Ord, Show)

parser :: Parser Input
parser = line `sepEndBy1` eol where
    line = (,) <$> some card <* hspace <*> decimal <* hspace
    card =  C2 <$ "2" <|> C3 <$ "3" <|> C4 <$ "4"
        <|> C5 <$ "5" <|> C6 <$ "6" <|> C7 <$ "7"
        <|> C8 <$ "8" <|> C9 <$ "9" <|>  T <$ "T"
        <|>  J <$ "J" <|>  Q <$ "Q" <|>  K <$ "K"
        <|>  A <$ "A"

handType :: [Card] -> HandType
handType cards = case cards' of
    [(5, _)] -> Five
    [(4, _), _] -> Four
    [(3, _), _] -> Full
    [(3, _), _, _] -> Three
    [(2, _), (2, _), _] -> TwoPair
    ((2, _) : _) -> OnePair
    _ -> HighCard
    where cards' = sortBy (comparing Down) . map (\(x, y) -> (y, x)) . Map.toList . freqs' $ cards

newtype Card' = Card' Card deriving (Eq, Show)

instance Ord Card' where
    compare (Card' J) (Card' J) = EQ
    compare (Card' J) _ = LT
    compare _ (Card' J) = GT
    compare (Card' c1) (Card' c2) = compare c1 c2

handType' :: [Card] -> HandType
handType' cards = case cards' of
    [(5, _)] -> Five
    [(4, _), (1, J)] -> Five
    [(4, J), _] -> Five
    [(3, _), (2, J)] -> Five
    [(3, J), _] -> Five
    [(4, _), _] -> Four
    ((3, _) : xs) | (1, J) `elem` xs -> Four
    [(2, _), (2, J), _] -> Four
    [(2, J), (2, _), _] -> Four
    [(3, _), _] -> Full
    [(2, _), (2, _), (1, J)] -> Full
    ((3, _) : _) -> Three
    ((2, J) : _) -> Three
    ((2, _) : xs) | (1, J) `elem` xs -> Three
    [(2, _), (2, _), _] -> TwoPair
    ((2, _) : _) -> OnePair
    _ | (1, J) `elem` cards' -> OnePair
      | otherwise -> HighCard
    where cards' = sortBy (comparing Down) . map (\(x, y) -> (y, x)) . Map.toList . freqs' $ cards

solveWith :: Ord a => ([Card] -> a) -> Input -> Int
solveWith order input = sum $ zipWith (*) [1..] bids where
    bids = map snd . sort . map (first order) $ input

part1 :: Input -> Int
part1 = solveWith \cards -> (handType cards, cards)

part2 :: Input -> Int
part2 = solveWith \cards -> (handType' cards, map Card' cards)

solve :: MonadIO m => Text -> m ()
solve = aoc parser part1 part2