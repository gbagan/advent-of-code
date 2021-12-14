module Day08 (solve) where
import           Data.Char (chr, ord)
import           Data.List (find, permutations)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import           Data.Set (Set)
import qualified Data.Set as Set
import           Text.Megaparsec (sepEndBy1, some)
import qualified Text.Megaparsec.Char as P
import           Text.Read (readMaybe)
import           Util (Parser, aocTemplate)

type Digit = IntSet
data Line = Line (Set Digit) [Digit]

parser :: Parser [Line]
parser = sepEndBy1 line P.eol where
    segment = (\c -> ord c - ord 'a') <$> P.lowerChar
    digit = IntSet.fromList <$> some segment
    digits = sepEndBy1 digit P.hspace1
    line = Line <$> (Set.fromList <$> digits) <* P.string "| " <*> digits

part1 :: [Line] -> Int
part1 xs = length $ xs >>= \(Line _ r) -> filter (\w -> IntSet.size w `elem` [2, 3, 4, 7]) r

digitList :: [Digit]
digitList = map IntSet.fromList
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
applyPerm p = IntSet.map (p!!)

decodeLine :: Line -> Maybe Int
decodeLine (Line l r) = do
    perm <- find (\p -> Set.map (applyPerm p) l == digitSet) (permutations [0..7])
    let permR = map (applyPerm perm) r
    strR <- traverse (`Map.lookup` digitMap) permR
    readMaybe strR

part2 :: [Line] -> Maybe Int
part2 xs = sum <$> traverse decodeLine xs

solve :: String -> IO ()
solve = aocTemplate parser (Just . part1) part2