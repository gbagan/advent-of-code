module Day08 (solve) where
import           RIO hiding (some)
import           RIO.Char (ord)
import           RIO.Char.Partial (chr)
import           RIO.List (find, permutations)
import           RIO.List.Partial ((!!))
import qualified RIO.Map as Map
import qualified Data.IntSet as IS
import qualified RIO.Set as Set
import           Text.Megaparsec (sepEndBy1, some)
import           Text.Megaparsec.Char (eol, lowerChar, hspace1, string)
import           Util (Parser, aoc)

type Digit = IntSet
data Line = Line !(Set Digit) ![Digit]

parser :: Parser [Line]
parser = sepEndBy1 line eol where
    segment = (\c -> ord c - ord 'a') <$> lowerChar
    digits = (IS.fromList <$> some segment) `sepEndBy1` hspace1
    line = Line <$> (Set.fromList <$> digits) <* string "| " <*> digits

part1 :: [Line] -> Int
part1 xs = length $ xs >>= \(Line _ r) -> filter (\w -> IS.size w `elem` [2, 3, 4, 7]) r

digitList :: [Digit]
digitList = map IS.fromList
    [ [0,1,2,4,5,6]
    , [2,5]
    , [0,2,3,4,6]
    , [0,2,3,5,6]
    , [1,2,3,5]
    , [0,1,3,5,6]
    , [0,1,3,4,5,6]
    , [0,2,5]
    , [0,1,2,3,4,5,6]
    , [0,1,2,3,5,6]
    ]

digitMap :: Map Digit Char
digitMap = Map.fromList $ zip digitList $ map (chr . (+ ord '0')) [0..]

digitSet :: Set Digit
digitSet = Set.fromList digitList

applyPerm :: [Int] -> Digit -> Digit
applyPerm p = IS.map (p!!)

decodeLine :: Line -> Maybe Int
decodeLine (Line l r) = do
    perm <- find (\p -> Set.map (applyPerm p) l == digitSet) (permutations [0..7])
    let permR = map (applyPerm perm) r
    strR <- traverse (`Map.lookup` digitMap) permR
    readMaybe strR

part2 :: [Line] -> Maybe Int
part2 xs = sum <$> traverse decodeLine xs

solve :: MonadIO m => Text -> m ()
solve = aoc parser part1 part2