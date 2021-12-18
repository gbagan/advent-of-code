module Day18 (solve) where
import           Control.Monad ((<=<))
import           Data.Bifunctor (bimap, first)
import           Text.Megaparsec (sepEndBy1, (<|>))
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as L
import           Util (Parser, aocTemplate)

data Snailfish = Leaf Int | Node Snailfish Snailfish deriving (Eq)

parser :: Parser [Snailfish]
parser = snailfish `sepEndBy1` P.eol where
    snailfish = Leaf <$> L.decimal 
            <|> Node <$> (P.char '[' *> snailfish <* P.char ',') <*> snailfish <* P.char ']'

explode :: Snailfish -> Either Snailfish Snailfish
explode = first (\(sf, _, _) -> sf) . go 0 where
    go :: Int -> Snailfish -> Either (Snailfish, Int, Int) Snailfish
    go _ (Leaf n) = Right (Leaf n)
    go depth (Node (Leaf l) (Leaf r)) | depth >= 4 = Left (Leaf 0, l, r)
    go depth (Node l r) = case go (depth + 1) l of
        Left (l', a, b) -> Left (Node l' (addToLeft b r), a, 0)
        _               -> case go (depth + 1) r of
            Left (r', a, b) -> Left (Node (addToRight a l) r', 0, b)
            _               -> Right (Node l r)
    addToLeft n (Leaf x)    = Leaf (x + n)
    addToLeft n (Node l r)  = Node (addToLeft n l) r
    addToRight n (Leaf x)   = Leaf (x + n)
    addToRight n (Node l r) = Node l (addToRight n r)

split :: Snailfish -> Either Snailfish Snailfish
split (Leaf n)
    | n >= 10   = Left $ Node (Leaf $ n `div` 2) (Leaf $ (n + 1) `div` 2)
    | otherwise = Right (Leaf n)
split (Node l r) = case split l of
    Left l' -> Left (Node l' r)
    _       -> bimap (Node l) (Node l) (split r)

reduce :: Snailfish -> Snailfish
reduce = either reduce id . (split <=< explode)

magnitude :: Snailfish -> Int
magnitude (Leaf n)   = n
magnitude (Node l r) = 3 * magnitude l + 2 * magnitude r

add :: Snailfish -> Snailfish -> Snailfish
add l r = reduce (Node l r)

part1 :: [Snailfish] -> Int
part1 = magnitude . foldl1 add

part2 :: [Snailfish] -> Int
part2 sfs = maximum [magnitude $ add a b | a <- sfs, b <- sfs, a /= b]

solve :: String -> IO ()
solve = aocTemplate parser (pure . part1) (pure . part2)