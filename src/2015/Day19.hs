-- https://adventofcode.com/2015/day/19
module Day19 (solve) where
import           AOC.Prelude hiding (fromList)
import           AOC (aoc)
import           AOC.Parser (Parser, letterChar, lowerChar, many, some, upperChar,sepEndBy1, eol)
import qualified Data.HashMap.Strict as Map
import qualified Data.Text as Text
import           Data.Massiv.Array hiding (map, mapM, zip)
import qualified Data.Massiv.Array as A
import           AOC.List (minimumMaybe)

type Atom = Text
type Molecule = [Atom]
type Rules = HashMap Atom [[Atom]]
data Input = Input Rules Molecule
data NormalizedInput = NormInput (Array B Ix1 [[Int]])  (Array U Ix1 Int) Int

parser :: Parser Input
parser = Input <$> rules <* eol <*> molecule where
    rule = (,) <$> (Text.pack <$> some letterChar) <* " => " <*> (pure <$> some atom)
    rules = Map.fromListWith (++) <$> rule `sepEndBy1` eol
    molecule = some atom
    atom = Text.pack <$> ((:) <$> upperChar <*> many lowerChar)

replacements :: Rules -> Molecule -> [Molecule]
replacements _ [] = []
replacements rules (x:xs) =
    [ y ++ xs | y <- Map.findWithDefault [] x rules]
    ++ ((x:) <$> replacements rules xs)

part1 :: Input -> Int
part1 (Input rules molecule) = length . hashNub . map Text.concat $ replacements rules molecule

binarizeRules :: Rules -> Rules
binarizeRules rules = rules where


    binarizeRules' :: (Atom, [[Atom]]) -> State Int [(Atom, [[Atom]])]
    binarizeRules' (key, rules') = concat <$> mapM (binarizeSequence key) rules'

    binarizeSequence :: Atom -> [Atom] -> State Int [(Atom, [[Atom]])]
    binarizeSequence key atoms = case atoms of
        [] -> error "binarize sequence: cannot happen"
        [a] -> pure [(key, [[a]])]
        [a1, a2] -> pure [(key, [[a1, a2]])]
        (a1 : a2 : atoms') -> do
            n <- get
            modify' (+1)
            let nonTerm = "nontem" <> show n
            rule <- binarizeSequence key (nonTerm : atoms')
            pure $ (nonTerm, [[a1, a2]]) : rule

completeRules :: Rules -> Rules
completeRules rules = Map.union rules (Map.fromList [ (key, []) | key <- keys]) where
    keys = hashNub $ concat (concat (Map.elems rules))

normalizeInput :: Input -> NormalizedInput
normalizeInput (Input rules molecule) = NormInput rules'' molecule' (dict Map.! "e") where
    rules' = completeRules rules
    keys = Map.keys rules'
    vkeys = fromList @B Seq keys
    dict = Map.fromList $ zip keys [0..]
    molecule' = fromList Seq [ dict Map.! atom | atom <- molecule]
    rules'' = compute $ A.map normalizeRule vkeys
    normalizeRule = map (map (dict Map.!)) . (rules' Map.!)

part2 :: Input -> Maybe Int
part2 input = arr ! Ix3 0 (n-1) eIdx where
    NormInput rules molecule eIdx = normalizeInput input
    Sz n = size molecule
    Sz m = size rules
    arr = makeArray @BL Seq (Sz3 n n m) \(Ix3 start end ruleIdx) ->
        if start == end && (molecule ! start) == ruleIdx
            then Just 0
            else fmap (+1)
                . minimumMaybe
                . mapMaybe (cost start end)
                $ rules ! ruleIdx

    cost start end = \case
        [] -> Nothing
        [x] -> arr ! Ix3 start end x
        (x:xs) -> minimumMaybe
                    [ cost1 + cost2
                    | split  <- [start .. end - length xs]
                    , cost1 <- maybeToList (arr ! Ix3 start split x)
                    , cost2 <- maybeToList (cost (split + 1) end xs)
                    ]

solve :: Text -> IO ()
solve = aoc parser part1 part2