-- https://adventofcode.com/2022/day/11
module AOC2022.Day11 (solve) where
import           RIO
import           RIO.Lens (ix)
import           RIO.List (genericLength, iterate, sort)
import           RIO.List.Partial ((!!))
import           Lens.Micro.TH (makeLenses)
import           Text.Megaparsec (anySingle, manyTill, sepBy1, sepEndBy1)
import           Text.Megaparsec.Char (eol, string)
import           Text.Megaparsec.Char.Lexer (decimal)
import           Util (Parser, aoc, takeEnd)

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
        _ <- manyTill anySingle eol
        _items <- string "  Starting items: " *> (decimal `sepBy1` string ", ") <* eol
        _operation <- string "  Operation: new = old " *> op <* eol
        _divBy <- string "  Test: divisible by " *> decimal <* eol
        _ifTrue <- string "    If true: throw to monkey " *> decimal <* eol
        _ifFalse <- string "    If false: throw to monkey " *> decimal <* eol
        let _inspected = 0
        pure $ Monkey {..}
    op = sqrOp <|> binop <*> decimal 
    sqrOp = (\x -> x * x)  <$ string "* old"
    binop = (+) <$ string "+ " <|> (*) <$ string "* "

solve' :: Part -> Int -> [Monkey] -> Integer
solve' part nbRounds mks = monkeyBusiness $ iterate runRound mks !! nbRounds where
    runRound monkeys = foldl' runMonkey monkeys [0 .. length monkeys - 1]

    runMonkey monkeys i = let ithMonkey = monkeys !! i in
                            foldl' (throwItem ithMonkey) monkeys (_items ithMonkey)
                            & ix i . items .~ []
                            & ix i . inspected %~ (+ genericLength (_items ithMonkey))

    throwItem sender monkeys item = monkeys & ix receiver . items %~ (newItem :) where
        newItem = (if part == Part1 then (`div` 3) else (`mod` modulo)) $ (sender ^. operation) item
        receiver = if newItem `mod` (sender ^. divBy) == 0 then sender ^. ifTrue else sender ^. ifFalse
    
    modulo = product $ map _divBy mks

    monkeyBusiness = product . takeEnd 2 . sort . map _inspected

solve :: MonadIO m => Text -> m ()
solve = aoc parser (solve' Part1 20) (solve' Part2 10000)