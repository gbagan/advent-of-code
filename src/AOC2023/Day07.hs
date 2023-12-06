-- https://adventofcode.com/2023/day/7
module AOC2023.Day07 (solve) where
import           RIO hiding (some)
import           RIO.List (sort, sortBy)
import qualified RIO.Map as Map
import           Text.Megaparsec (sepEndBy1, some)
import           Text.Megaparsec.Char (eol, hspace)
import           Text.Megaparsec.Char.Lexer (decimal)
import           Util (Parser, aoc, count, freqs')

type Bid = ([Card], Int)
type Input = [Bid]
data Card = C2 | C3 | C4 | C5 |  C6 | C7 | C8 | C9 | T | J | Q | K | A deriving (Eq, Ord)
data HandType = HighCard | OnePair | TwoPair | Three |  Full | Four | Five deriving (Eq, Ord)
newtype Card' = Card' Card deriving (Eq)

instance Ord Card' where
    compare (Card' J) (Card' J) = EQ
    compare (Card' J) _ = LT
    compare _ (Card' J) = GT
    compare (Card' c1) (Card' c2) = compare c1 c2

parser :: Parser Input
parser = line `sepEndBy1` eol where
    line = (,) <$> some card <* hspace <*> decimal <* hspace
    card =  C2 <$ "2" <|> C3 <$ "3" <|> C4 <$ "4"
        <|> C5 <$ "5" <|> C6 <$ "6" <|> C7 <$ "7"
        <|> C8 <$ "8" <|> C9 <$ "9" <|>  T <$ "T"
        <|>  J <$ "J" <|>  Q <$ "Q" <|>  K <$ "K"
        <|>  A <$ "A"

cardFreq :: [Card] -> [Int]
cardFreq = sortBy (comparing Down) . Map.elems . freqs'

handType' :: [Int] -> HandType
handType' = \case
    [5] -> Five
    (4:_) -> Four
    [3, 2] -> Full
    (3:_) -> Three
    (2:2:_) -> TwoPair
    (2:_) -> OnePair
    _ -> HighCard

    
handType :: [Card] -> HandType
handType = handType' . cardFreq

handType2 :: [Card] -> HandType
handType2 cards = handType' . addJokers . cardFreq . filter (/=J) $ cards where
    nbJokers = count (==J) cards
    addJokers [] = [nbJokers]
    addJokers (c:cs) = c+nbJokers : cs


solveWith :: Ord a => ([Card] -> a) -> Input -> Int
solveWith order input = sum $ zipWith (*) [1..] bids where
    bids = map snd . sort . map (first order) $ input

part1 :: Input -> Int
part1 = solveWith \cards -> (handType cards, cards)

part2 :: Input -> Int
part2 = solveWith \cards -> (handType2 cards, map Card' cards)

solve :: MonadIO m => Text -> m ()
solve = aoc parser part1 part2