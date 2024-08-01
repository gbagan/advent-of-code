module Day08 (solve) where
import           AOC.Prelude
import           Data.List ((!!))
import qualified Data.Map.Strict as Map
import qualified Data.IntSet as IS
import qualified Data.Set as Set
import           Text.Megaparsec ()
import           Text.Megaparsec.Char ()
import           AOC (aoc)
import           AOC.Parser (Parser, eol, lowerChar, hspace, sepEndBy1, some)

type Digit = IntSet
data Line = Line !(Set Digit) ![Digit]

parser :: Parser [Line]
parser = sepEndBy1 line eol where
    segment = (\c -> ord c - ord 'a') <$> lowerChar
    digits = (IS.fromList <$> some segment) `sepEndBy1` hspace
    line = Line . Set.fromList <$> digits <* "| " <*> digits

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

solve :: Text -> IO ()
solve = aoc parser part1 part2