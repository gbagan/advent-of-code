-- https://adventofcode.com/2023/day/19
module AOC2023.Day19 (solve) where
import           AOC.Prelude hiding (LT, GT)
import qualified Data.HashMap.Strict as Map
import           Lens.Micro (Lens', set, (^.))
import           Lens.Micro.TH (makeLensesFor)
import           AOC (aoc)
import           AOC.Parser (Parser, sepBy1,sepEndBy1, eol, choice, decimal, lowerChar, some, try)

data Rating a = Rating { _x :: !a, _m :: !a, _a :: !a, _s :: !a}
data Category = X | M | A | S
data Test = LT Category Int | GT Category Int | Otherwise
data Instr = Accept | Reject | Goto String
data Step = Step !Test !Instr
type Workflows = HashMap String [Step]
data Input = Input !Workflows ![Rating Int]
type RatingRange = Rating (Int, Int)

makeLensesFor [("_x", "xL"), ("_m", "mL"), ("_a", "aL"), ("_s", "sL")] ''Rating

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

catLens :: Category -> (forall a. Lens' (Rating a) a)
catLens = \case
    X -> xL
    M -> mL
    A -> aL
    S -> sL

part1 :: Input -> Int
part1 (Input workflows ratings) = sum . map score $ filter accepts ratings where
    accepts rating = go "in" where
        go name = case passTests rating steps of
            Accept -> True
            Reject -> False
            Goto name' -> go name'   
            where steps = workflows Map.! name
    passTests _ [] = error "passTests: cannot happen"
    passTests rating ((Step test instr):steps) = case test of
        Otherwise -> instr
        LT cat n | rating ^. catLens cat < n -> instr
        GT cat n | rating ^. catLens cat > n -> instr 
        _  -> passTests rating steps
    score (Rating x m a s) = x + m + a + s

splitRatings :: Test -> [RatingRange] -> ([RatingRange], [RatingRange])
splitRatings test = partitionEithers . concatMap (splitRating test) where
    splitRating Otherwise rating = [Right rating]
    splitRating (LT cat n) rating =
        let (min_, max_) = rating ^. catLens cat in
        if | min_ >= n -> [Left rating]
           | max_ < n -> [Right rating]
           | otherwise -> [ Right $ set (catLens cat) (min_, n-1) rating
                          , Left $ set (catLens cat) (n, max_) rating
                          ]
    splitRating (GT cat n) rating = 
        let (min_, max_) = rating ^. catLens cat in
        if | max_ <= n -> [Left rating]
           | min_ > n -> [Right rating]
           | otherwise -> [ Right $ set (catLens cat) (n+1, max_) rating
                          , Left $ set (catLens cat) (min_, n) rating
                          ]

part2 :: Input -> Int
part2 (Input workflows _) = sum . map size $ go initRanges (workflows Map.! "in")
    where
    initRanges = [Rating (1, 4000) (1, 4000) (1, 4000) (1, 4000)]
    go _ [] = error "part2: cannot happen"
    go ratings (Step test instr : steps) = ratings' where
        (failed, succeeded) = splitRatings test ratings
        succeeded' = case instr of
                Accept -> succeeded
                Reject -> []
                Goto name -> go succeeded (workflows Map.! name)
        ratings' = if null failed then succeeded' else succeeded' ++ go failed steps

    size (Rating (xmin, xmax) (mmin, mmax) (amin, amax) (smin, smax)) =
        (xmax - xmin + 1) * (mmax - mmin + 1) * (amax - amin + 1) * (smax - smin + 1)

solve :: Text -> IO ()
solve = aoc parser part1 part2