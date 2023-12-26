-- https://adventofcode.com/2022/day/11
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module Day11 (solve) where
import           AOC.Prelude
import           Data.List ((!!))
import           Lens.Micro ((^.), (.~), (%~), ix)
import           Lens.Micro.TH (makeLenses)
import           AOC (aoc)
import           AOC.Parser (Parser, sepBy1, sepEndBy1, eol, decimal, skipLine)

data Monkey = Monkey {
        _items :: ![Integer]
    ,   _operation :: !(Integer -> Integer)
    ,   _divBy :: !Integer
    ,   _ifTrue :: !Int
    ,   _ifFalse :: !Int
    ,   _inspected :: !Integer
}

makeLenses ''Monkey

data Part = Part1 | Part2 deriving (Eq)

parser :: Parser [Monkey]
parser = monkey `sepEndBy1` eol where
    monkey = do
        skipLine
        _items <- "  Starting items: " *> (decimal `sepBy1` ", ") <* eol
        _operation <- "  Operation: new = old " *> op <* eol
        _divBy <- "  Test: divisible by " *> decimal <* eol
        _ifTrue <- "    If true: throw to monkey " *> decimal <* eol
        _ifFalse <- "    If false: throw to monkey " *> decimal <* eol
        let _inspected = 0
        pure $ Monkey {..}
    op = sqrOp <|> binop <*> decimal 
    sqrOp = (\x -> x * x)  <$ "* old"
    binop = (+) <$ "+ " <|> (*) <$ "* "

solve' :: Part -> Int -> [Monkey] -> Integer
solve' part nbRounds mks = monkeyBusiness $ iterate' runRound mks !! nbRounds where
    runRound monkeys = foldl' runMonkey monkeys [0 .. length monkeys - 1]

    runMonkey monkeys i = let ithMonkey = monkeys !! i in
                            foldl' (throwItem ithMonkey) monkeys (_items ithMonkey)
                            & ix i . items .~ []
                            & ix i . inspected %~ (+ genericLength (_items ithMonkey))

    throwItem sender monkeys item = monkeys & ix receiver . items %~ (newItem :) where
        newItem = (if part == Part1 then (`div` 3) else (`mod` modulo)) $ _operation sender item
        receiver = if newItem `mod` (sender ^. divBy) == 0 then _ifTrue sender else _ifFalse sender
    
    modulo = product $ map _divBy mks

    monkeyBusiness = product . take 2 . sortOn Down . map _inspected

solve :: Text -> IO ()
solve = aoc parser (solve' Part1 20) (solve' Part2 10000)