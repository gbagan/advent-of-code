-- https://adventofcode.com/2018/day/24
module Day24 (solve) where
import           AOC.Prelude hiding (round)
import           AOC (aoc)
import           AOC.Parser (Parser, sepEndBy1, eol, decimal, lowerChar, optional, sepBy1, some, scanf)
import qualified Data.IntMap as Map
import           AOC.List (maximumOnMaybe)
import           AOC.Util (minBinSearch)

data Units = Units
    { unitType :: !UnitType
    , unitNumber :: !Int
    , hitPoints :: !Int
    , immunities :: [String]
    , weakness :: [String]
    , attackDamage :: !Int
    , attackType :: String 
    , initiative :: !Int
    } deriving (Eq, Show)

data UnitType = ImmuneSystem | Infection deriving (Eq, Ord, Show)
data Outcome = Win | Lose | Draw deriving (Show)
type Input = IntMap Units

parser :: Parser Input
parser = do
    _ <- "Immune System:" <* eol
    immuneSystem <- units ImmuneSystem `sepEndBy1` eol
    _ <- eol <* "Infection:" <* eol
    infection <- units Infection `sepEndBy1` eol
    pure . Map.fromList . zip [0..] $ immuneSystem++infection
    where
    units type_  = do
        (n, hp, (imm, w), att, atype, init_)
            <- [scanf|{d} units each with {d} hit points {immuneWeak}with an attack that does {d} {name} damage at initiative {d}|]
        pure $ Units type_ n hp imm w att atype init_
    immuneWeak = fromMaybe ([], []) <$> optional ("(" *> immuneWeak' <* ") ")
    immuneWeak' = f <$> ((,) <$> name <* " to " <*>  (name `sepBy1` ", ")) `sepEndBy1` "; "
    name = some lowerChar  
    d = decimal
    f [("immune", xs)] = (xs, [])
    f [("weak", xs)] = ([], xs)
    f [("immune", xs), ("weak", ys)] = (xs, ys)
    f [("weak", ys), ("immune", xs)] = (xs, ys)
    f _ = ([], [])

effectivePower :: Units -> Int
effectivePower u = u.attackDamage * u.unitNumber

attackDamge :: Units -> Units -> Int
attackDamge attacker defender
    | attacker.attackType `elem` defender.immunities = 0
    | attacker.attackType `elem` defender.weakness = 2 * effectivePower attacker
    | otherwise = effectivePower attacker

attack :: Units -> Units -> Units
attack attacker defender = defender{unitNumber=defender.unitNumber - attackDamge attacker defender `div` defender.hitPoints}

targetSelectionOrdering :: Input -> [(Int, Units)]
targetSelectionOrdering units =
    sortOn
        (\(_, unit) -> (- (effectivePower unit), -unit.initiative))
        (Map.toList units)

targetSelection :: Units -> Input -> Maybe (Int, Units)
targetSelection attacker units =
    case maximumOnMaybe (criterion . snd) targets of
        Nothing -> Nothing
        Just (idx, target) | attackDamge attacker target == 0 -> Nothing
                           | otherwise -> Just (idx, target)
    where
    targets = [ (idx, unit) | (idx, unit) <- Map.toList units, unit.unitType /= attacker.unitType]
    criterion target = (attackDamge attacker target, effectivePower target, target.initiative)


fights :: Input -> [(Int, Int)]
fights units = 
        map (\(idx, _, idx', _) -> (idx, idx'))
        . sortOn (\(_, a, _, _) -> - a.initiative)
        . fst 
        $ foldl' go ([], units) (targetSelectionOrdering units) where
    go (acc, units') (idx, attacker) = case targetSelection attacker units' of
        Nothing -> (acc, units')
        Just (idx', target) -> ((idx, attacker, idx', target) : acc, Map.delete idx' units')

round :: Input -> Input
round input = foldl' go input (fights input) where
    go units (attackerIdx, targetIdx) = case units Map.!? attackerIdx of
        Nothing -> units
        Just attacker -> units' where
            target = units Map.! targetIdx
            target' = attack attacker target
            units' = if target'.unitNumber <= 0
                        then Map.delete targetIdx units
                        else Map.insert targetIdx target' units 

outcome :: IntMap Units -> (Outcome, Int)
outcome units = (outcome_, scr) where
    scr = sum . fmap unitNumber $ Map.elems units
    outcome_ = case ordNub . fmap unitType $ Map.elems units of
        [ImmuneSystem] -> Win
        [Infection] -> Lose
        _ -> Draw

boost :: Int -> Input -> Input
boost n = Map.map \unit -> if unit.unitType == ImmuneSystem 
                                then unit{attackDamage=unit.attackDamage+n}
                                else unit

simulate :: Input -> Input
simulate units
    | units == units' = units
    | otherwise = simulate units'
    where units' = round units

part1 :: Input -> Int
part1 = snd . outcome . simulate . boost 1000

part2 :: Input -> Int
part2 units = minBinSearch f 0 Nothing where
    f n = case outcome . simulate $ boost n units of
        (Win, score) -> Just score
        _ -> Nothing

solve :: Text -> IO ()
solve = aoc parser part1 part2