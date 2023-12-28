-- https://adventofcode.com/2015/day/22

{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Day22 (solve) where
import           AOC.Prelude hiding (State, state)
import           AOC (aoc)
import           AOC.Parser (Parser, decimal, eol, format)

data State = State { playerHP, bossHP, bossDamage, consumedMana, currentMana
                   , poison, recharge, shield :: !Int, hardMode :: !Bool
                   }

data Spell = Poison | Recharge | Shield | Missile | Drain | BossAttack deriving (Eq, Show)

min' :: Maybe Int -> Maybe Int -> Maybe Int
min' Nothing x = x
min' x Nothing = x
min' (Just x) (Just y) | x <= y = Just x
                       | otherwise = Just y

parser :: Parser (Int, Int)
parser = do
    hp <- [format|Hit Points: {decimal}|] <* eol
    damage <- [format|Damage: {decimal}|]
    pure (hp, damage)

spellCost :: Spell -> Int
spellCost Poison     = 173
spellCost Recharge   = 229
spellCost Shield     = 113
spellCost Missile    = 53
spellCost Drain      = 73
spellCost BossAttack = 0

turn :: Spell -> State -> State
turn spell state =
    let bossHP = state.bossHP
            - (if state.poison > 0 then 3 else 0)
            - (if spell == Missile then 4 else 0)
            - (if spell == Drain then 2 else 0)
        playerHP = state.playerHP 
                - (if state.hardMode then 1 else 0)
                - (if | spell /= BossAttack -> 0
                      | state.shield > 0    -> max 0 (state.bossDamage - 7)
                      | otherwise           -> state.bossDamage
                  )
                + (if spell == Drain then 2 else 0)
        cost = spellCost spell
    in
        state
            { playerHP = playerHP
            , bossHP = bossHP
            , consumedMana = state.consumedMana + cost
            , currentMana = state.currentMana
                - cost
                + (if state.recharge > 0 then 101 else 0)
            , poison = if spell == Poison then 6 else max 0 (state.poison - 1)
            , recharge = if spell == Recharge then 5 else max 0 (state.recharge - 1)
            , shield = if spell == Shield then 6 else max 0 (state.shield - 1)
            }

simulate :: Maybe Int -> State -> Maybe Int
simulate bestMana state =
    foldl' go bestMana [Poison, Recharge, Shield, Missile, Drain]
    where
    go mana spell = min' mana do
        let cost = spellCost spell
        let consumedMana = state.consumedMana + cost
        guard $ state.playerHP > if state.hardMode then 1 else 0
        guard $ cost <= state.currentMana  -- +  (if state.recharge > 0 then 101 else 0)
        guard $ isNothing mana || Just consumedMana < mana
        guard $ spell /= Poison || state.poison <= 1
        guard $ spell /= Recharge || state.recharge <= 1
        guard $ spell /= Shield || state.shield <= 1
        let state' = turn spell state
        if state'.bossHP <= 0
            then Just consumedMana
            else simulateBoss mana state'

simulateBoss :: Maybe Int -> State -> Maybe Int
simulateBoss bestMana state =
    let state' = turn BossAttack state
        thr = if state'.hardMode then 1 else 0
    in
    if | state'.bossHP <= 0     -> Just $ state'.consumedMana 
       | state'.playerHP <= thr -> Nothing
       | otherwise               -> simulate bestMana state'

solveFor :: Bool -> (Int, Int) -> Maybe Int
solveFor hardMode (bossHP, bossDamage) = simulate Nothing state
    where state = State { hardMode = hardMode
                        , playerHP = 50
                        , bossHP = bossHP
                        , bossDamage = bossDamage
                        , consumedMana = 0
                        , currentMana = 500
                        , poison = 0
                        , recharge = 0
                        , shield = 0
                        }

solve :: Text -> IO ()
solve = aoc parser (solveFor False) (solveFor True)