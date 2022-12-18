module Day23 (solve) where
import           RIO
import           RIO.List.Partial (head, last)
import qualified RIO.Vector as Vec
import           RIO.Vector.Partial ((!), (//))
import qualified Data.IntMap as IM
import           Text.Megaparsec (anySingle)
import           Text.Megaparsec.Char (char)
import           Algorithm.Search (dijkstraAssoc)
import           Util (Parser, aoc)

data Amphipod = Amber | Bronze | Copper | Desert deriving (Eq, Ord, Enum)
type Rooms = Vector [Amphipod]
type Hall = IntMap Amphipod
type Board = (Rooms, Hall)

boardFromAmphipods :: [Amphipod] -> Maybe Board
boardFromAmphipods [a1, b1, c1, d1, a2, b2, c2, d2] =
                    Just (Vec.fromList [[a1, a2], [b1, b2], [c1, c2], [d1, d2]], IM.empty) 
boardFromAmphipods _ = Nothing

parser :: Parser Board
parser = do
    board <- boardFromAmphipods <$> amphipods
    maybe (fail "Invalid board") pure board
    where
    amphipod :: Parser Amphipod
    amphipod = char 'A' $> Amber 
           <|> char 'B' $> Bronze
           <|> char 'C' $> Copper
           <|> char 'D' $> Desert
    amphipod' = Just <$> amphipod <|> anySingle $> Nothing
    amphipods = catMaybes <$> some amphipod'

moveCost :: Amphipod -> Int
moveCost Amber = 1
moveCost Bronze = 10
moveCost Copper = 100
moveCost Desert = 1000

roomNo :: Amphipod -> Int
roomNo = fromEnum

hallPositions :: [Int]
hallPositions = [0, 1, 3, 5, 7, 9, 10]

roomPosition :: Amphipod -> Int
roomPosition x = 2 * fromEnum x + 2

freePath :: Int -> Int -> IntMap a -> Bool
freePath n m mp | n < m     = maybe 20 fst (IM.lookupGT n mp) > m
                | otherwise = maybe (-1) fst (IM.lookupLT n mp) < m

enterRoom :: Int -> Board -> Int -> Maybe (Board, Int)
enterRoom size (rooms, hall) h =
    case hall IM.!? h of
        Nothing -> Nothing
        Just c ->
            let room = rooms ! roomNo c in
            if length room < size && all (== c) room && freePath h (roomPosition c) hall
                then let newBoard = (rooms // [(roomNo c, c : rooms ! roomNo c)], IM.delete h hall)
                         cost = moveCost c * (size - length room + abs (h - roomPosition c))
                     in Just (newBoard, cost)
                else Nothing

roomEnters :: Int -> Board -> [(Board, Int)]
roomEnters size chart = catMaybes [enterRoom size chart h | h <- hallPositions]

exitRoom :: Int -> Board -> Amphipod -> Int -> Maybe (Board, Int)
exitRoom size (rooms, hall) r h = case rooms ! roomNo r of
    [] -> Nothing
    (c:cs) | freePath (roomPosition r) h hall -> 
                let newBoard = (rooms // [(roomNo r, cs)], IM.insert h c hall)
                    cost = moveCost c * (size - length cs + abs (h - roomPosition r))
                in Just (newBoard, cost)
           | otherwise -> Nothing

roomExits :: Int -> Board -> [(Board, Int)]
roomExits size chart = catMaybes [exitRoom size chart r h | r <- [Amber,Bronze,Copper,Desert], h <- hallPositions]

neighbors :: Int -> Board -> [(Board, Int)]
neighbors size st = roomEnters size st ++ roomExits size st

finalBoard :: Int -> Board
finalBoard n = (Vec.fromList [replicate n Amber, replicate n Bronze, replicate n Copper, replicate n Desert], IM.empty)

part1 :: Board -> Maybe Int
part1 board = fst <$> dijkstraAssoc (neighbors 2) (== finalBoard 2) board 

extendBoard :: Board -> Board
extendBoard (rooms, hall) = (Vec.imap (\i room -> head room : middle i ++ [last room]) rooms, hall)
    where middle 0 = [Desert, Desert]
          middle 1 = [Copper, Bronze]
          middle 2 = [Bronze, Amber]
          middle _ = [Amber, Copper] 

part2 :: Board -> Maybe Int
part2 board = fst <$> dijkstraAssoc (neighbors 4) (== finalBoard 4) (extendBoard board) 

solve :: MonadIO m => Text -> m ()
solve = aoc parser part1 part2