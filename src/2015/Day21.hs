-- https://adventofcode.com/2015/day/21
module Day21 (solve) where
import           AOC.Prelude
import           Data.List (maximum, minimum)
import           AOC (aoc)
import           AOC.Parser (Parser, eol, scanf, decimal)

data Boss = Boss { _hp, _damage, _armor :: Int }
data Item = Item { _cost, _itemDamage, _itemArmor :: Int }

parser :: Parser Boss
parser = do
    hp <- [scanf| Hit Points: {decimal}|] <* eol
    damage <- [scanf| Damage: {decimal}|] <* eol
    Boss hp damage <$> [scanf| Armor: {decimal}|]

weapons, armors, rings :: [Item]

weapons =
    [ Item 8  4 0
    , Item 10 5 0
    , Item 25 6 0
    , Item 40 7 0
    , Item 74 8 0
    ]

armors =
    [ Item 13  0 1
    , Item 31  0 2
    , Item 53  0 3
    , Item 75  0 4
    , Item 102 0 5
    ]

rings =
    [ Item 25  1 0
    , Item 50  2 0
    , Item 100 3 0
    , Item 20  0 1
    , Item 40  0 2
    , Item 80  0 3
    ]

noItem :: Item
noItem = Item 0 0 0

gears :: [[Item]]
gears =
    [ [weapon, armor, ring1, ring2]
    | weapon <- weapons
    , armor <- noItem : armors
    , ring1 <- noItem : rings
    , ring2 <- noItem : rings
    ]

playerWins :: [Item] -> Boss -> Bool
playerWins gear (Boss bossHp bossDamage bossArmor) =
    let playerDamage = sum (map _itemDamage gear) - bossArmor
        playerArmor = sum (map _itemArmor gear)
        bossDamage' = bossDamage - playerArmor
        nbTurnsToWin = (bossHp - 1) `quot` playerDamage
        nbTurnsToLose = 99 `quot` bossDamage'
    in playerDamage > 0 && (bossDamage' <= 0 || nbTurnsToWin <= nbTurnsToLose)

part1, part2 :: Boss -> Int
part1 boss = minimum [ sum (map _cost gear) | gear <- gears, playerWins gear boss]
part2 boss = maximum [ sum (map _cost gear) | gear <- gears, not (playerWins gear boss)]

solve :: Text -> IO ()
solve = aoc parser part1 part2
