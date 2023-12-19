-- https://adventofcode.com/2023/day/19
module AOC2023.Day19 (solve) where
import           AOC.Prelude hiding (LT, GT)
import qualified Data.HashMap.Strict as Map
import           AOC (aoc)
import           AOC.Parser (Parser, sepBy1,sepEndBy1, eol, choice, decimal, lowerChar, some, try)

data Rating a = Rating { _x :: !a, _m :: !a, _a :: !a, _s :: !a}
data Category = X | M | A | S
data Test = LT Category Int | GT Category Int | Otherwise
data Instr = Accept | Reject | Goto String
data Step = Step !Test !Instr
type Workflows = HashMap String [Step]
data Input = Input !Workflows ![Rating Int]

parser :: Parser Input
parser = Input . Map.fromList <$> workflows <* eol <*> ratings where
    workflows = workflow `sepEndBy1` eol
    ratings = rating `sepEndBy1` eol
    workflow = (,) <$> some lowerChar <* "{" <*> step `sepBy1` "," <* "}"
    step = try (Step <$> test <* ":" <*> instr) <|> (Step Otherwise <$> instr)
    test = do
        c <- category
        "<" *> (LT c <$> decimal) <|> ">" *> (GT c <$> decimal)
    instr = Accept <$ "A" <|> Reject <$ "R" <|> Goto <$> some lowerChar
    category = choice [X <$ "x", M <$ "m", A <$ "a", S <$ "s"]
    rating = do
        x <- "{x=" *> decimal
        m <- ",m=" *> decimal
        a <- ",a=" *> decimal
        s <- ",s=" *> decimal <* "}"
        pure $ Rating x m a s

categoryNumber :: Category -> Rating a -> a
categoryNumber = \case
    X -> _x
    M -> _m
    A -> _a
    S -> _s

setCategoryNumber :: Category -> a -> Rating a -> Rating a
setCategoryNumber = \case
    X -> \n r -> r{_x = n}
    M -> \n r -> r{_m = n}
    A -> \n r -> r{_a = n}
    S -> \n r -> r{_s = n}

part1 :: Input -> Int
part1 (Input workflows ratings) = sum [score rating | rating <- ratings, accepts rating]
    where
    accepts rating = go "in" where
        go name =
            let steps = workflows Map.! name in
            case passTests rating steps of
                Accept -> True
                Reject -> False
                Goto name' -> go name'   
    passTests _ [] = error "passTests: cannot happen"
    passTests rating ((Step test instr):steps) = case test of
        Otherwise -> instr
        LT cat n -> if categoryNumber cat rating < n 
                        then instr
                        else passTests rating steps
        GT cat n -> if categoryNumber cat rating > n 
                        then instr
                        else passTests rating steps
    score (Rating x m a s) = x + m + a + s

concatPairMap :: (a -> ([b], [c])) -> [a] -> ([b], [c])
concatPairMap _ [] = ([], [])
concatPairMap f (x:xs) = (ys ++ ys', zs ++ zs') where
        (ys, zs) = f x
        (ys', zs') = concatPairMap f xs

splitRatings :: Test -> [Rating (Int, Int)] -> ([Rating (Int, Int)], [Rating (Int, Int)])
splitRatings test = concatPairMap (splitRating test) where
    splitRating Otherwise rating = ([rating], [])
    splitRating (LT cat n) rating = 
        let (min_, max_) = categoryNumber cat rating in
        if | min_ >= n -> ([], [rating])
           | max_ < n -> ([rating], [])
           | otherwise -> ( [setCategoryNumber cat (min_, n-1) rating]
                          , [setCategoryNumber cat (n, max_) rating]
                          )
    splitRating (GT cat n) rating = 
        let (min_, max_) = categoryNumber cat rating in
        if | max_ <= n -> ([], [rating])
           | min_ > n -> ([rating], [])
           | otherwise -> ( [setCategoryNumber cat (n+1, max_) rating]
                          , [setCategoryNumber cat (min_, n) rating]
                          )

part2 :: Input -> Int
part2 (Input workflows _) = sum . map score $ go [Rating (1, 4000) (1, 4000) (1, 4000) (1, 4000)] (workflows Map.! "in")
    where
    go _ [] = error "part2 go: cannot happen"
    go ratings (Step test instr : steps) = ratings1' ++ ratings2' where
        (ratings1, ratings2) = splitRatings test ratings
        ratings1' = case instr of
                Accept -> ratings1
                Reject -> []
                Goto name -> go ratings1 (workflows Map.! name)
        ratings2' = if null ratings2 then [] else go ratings2 steps

    score (Rating (xmin, xmax)  (mmin, mmax)  (amin, amax)  (smin, smax)) =
        (xmax - xmin + 1) * (mmax - mmin + 1) * (amax - amin + 1) * (smax - smin + 1)

solve :: Text -> IO ()
solve = aoc parser part1 part2