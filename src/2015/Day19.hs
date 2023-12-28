-- https://adventofcode.com/2015/day/19
module Day19 (solve) where
import           AOC.Prelude hiding (fromList)
import           AOC (aoc)
import           AOC.Parser (Parser, letterChar, lowerChar, many, some, upperChar,sepEndBy1, eol)
import qualified Data.HashMap.Strict as Map
import qualified Data.Text as Text
import           Data.Massiv.Array hiding (map, mapM, zip)
import           AOC.List (minimumMaybe)

type Atom = Text
type Molecule = [Atom]
type Rules = HashMap Atom [[Atom]]
data Input = Input Rules Molecule
data NormalizedInput = NormInput (Array B Ix1 [[Int]])  (Array U Ix1 Int) Int

parser :: Parser Input
parser = Input <$> rules <* eol <*> molecule where
    rule = (,) <$> key <* " => " <*> (pure <$> some atom)
    rules = Map.fromListWith (++) <$> rule `sepEndBy1` eol
    molecule = some atom
    key = Text.pack <$> some letterChar
    atom = Text.pack <$> ((:) <$> upperChar <*> many lowerChar)

replacements :: Rules -> Molecule -> [Molecule]
replacements _ [] = []
replacements rules (x:xs) =
       [ y ++ xs | y <- Map.findWithDefault [] x rules]
    ++ ((x:) <$> replacements rules xs)

part1 :: Input -> Int
part1 (Input rules molecule) = length . hashNub . map Text.concat $ replacements rules molecule

completeRules :: Rules -> Rules
completeRules rules = Map.union rules (Map.fromList (map (,[]) keys)) where
    keys = hashNub $ concat (concat (Map.elems rules))

-- compute a new input where atoms are Int instead of Text
normalizeInput :: Input -> NormalizedInput
normalizeInput (Input rules molecule) = NormInput rules'' molecule' (dict Map.! "e")
    where
    rules' = completeRules rules
    keys = Map.keys rules'
    dict = Map.fromList $ zip keys [0..]
    molecule' = fromList Seq [ dict Map.! atom | atom <- molecule]
    rules'' = fromList Seq $ map normalizeRule keys
    normalizeRule = map (map (dict Map.!)) . (rules' Map.!)

part2 :: Input -> Maybe Int
part2 input = costArr ! Ix3 0 (n-1) eIdx where
    NormInput rules molecule eIdx = normalizeInput input
    Sz n = size molecule
    Sz m = size rules
    costArr = makeArray @BL Seq (Sz3 n n m) \(Ix3 start end ruleIdx) ->
        if start == end && (molecule ! start) == ruleIdx
            then Just 0
            else fmap (+1)
                . minimumMaybe
                . mapMaybe (cost start end)
                $ rules ! ruleIdx

    cost start end = \case
        [] -> Nothing
        [x] -> costArr ! Ix3 start end x
        (x:xs) -> minimumMaybe
                    [ cost1 + cost2
                    | split  <- [start .. end - length xs]
                    , cost1 <- maybeToList (costArr ! Ix3 start split x)
                    , cost2 <- maybeToList (cost (split + 1) end xs)
                    ]

solve :: Text -> IO ()
solve = aoc parser part1 part2