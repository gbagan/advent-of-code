module Day08 (solve) where
import           Data.Char (chr, ord)
import           Data.List (find, permutations)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Void (Void)
import           Text.Megaparsec (Parsec, parseMaybe, parseTest, sepEndBy1, some)
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as L
import           Text.Read (readMaybe)

type Parser = Parsec Void String
type Digit = Set Int
data Line = Line (Set Digit) [Digit]

parser :: Parser [Line]
parser = sepEndBy1 line P.eol where
    segment = (\c -> ord c - ord 'a') <$> P.lowerChar
    digit = Set.fromList <$> some segment
    digits = sepEndBy1 digit P.hspace1
    line = Line <$> (Set.fromList <$> digits) <* P.string "| " <*> digits

part1 :: [Line] -> Int
part1 xs = length $ xs >>= \(Line _ r) -> filter (\w -> length w `elem` [2, 3, 4, 7]) r 

digitList :: [Digit]
digitList = map Set.fromList
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

perms7 :: [[Int]]
perms7 = permutations [0..7]

applyPerm :: [Int] -> Digit -> Digit
applyPerm p = Set.map (p!!)

decodeLine :: Line -> Maybe Int
decodeLine (Line l r) = do
    perm <- find (\p -> Set.map (applyPerm p) l == digitSet) perms7
    let permR = map (applyPerm perm) r
    strR <- traverse (`Map.lookup` digitMap) permR
    readMaybe strR

part2 :: [Line] -> Maybe Int
part2 xs = sum <$> traverse decodeLine xs

solve :: String -> Maybe (Int, Int)
solve s = do
    ls <- parseMaybe parser s
    let r1 = part1 ls
    r2 <- part2 ls
    pure (r1, r2)
