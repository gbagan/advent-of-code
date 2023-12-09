-- https://adventofcode.com/2022/day/21
module AOC2022.Day21 (solve) where
import           Relude hiding (Op, some)
import qualified Data.HashMap.Strict as Map
import qualified Data.Text as Text
import           AOC (aoc)
import           AOC.Parser (Parser, sepEndBy1, eol, lowerChar, some, decimal)

data Job = JInt !Integer | Job !Text !Op !Text
data Op = Add | Sub | Mul | Div

parser :: Parser (HashMap Text Job)
parser = Map.fromList <$> line `sepEndBy1` eol where
    line = (,) <$> monkey <* ": " <*> job
    job = JInt <$> decimal <|> Job <$> monkey <*> op <*> monkey
    op = Add <$ " + " <|> Sub <$ " - "
        <|> Mul <$ " * " <|> Div <$ " / "
    monkey = Text.pack <$> some lowerChar

calc :: Text -> HashMap Text Job -> Maybe Integer
calc monkey m = case Map.lookup monkey m of
        Nothing -> Nothing
        Just (JInt n) -> Just n
        Just (Job mk1 op mk2) -> do
            v1 <- calc mk1 m
            v2 <- calc mk2 m
            Just $ interpretOp op v1 v2
    where
    interpretOp Add = (+)
    interpretOp Sub = (-)
    interpretOp Mul = (*)
    interpretOp Div = div

part1 :: HashMap Text Job -> Maybe Integer
part1 = calc "root"

part2 :: HashMap Text Job -> Maybe Integer
part2 m = solve' "root" 0 where
    m' = Map.delete "humn" . Map.adjust modifyRoot "root" $ m
    modifyRoot (Job mk1 _ mk2) = Job mk1 Sub mk2
    modifyRoot x = x

    solve' monkey val =
        case Map.lookup monkey m' of
            Nothing -> Just val
            Just (Job mk1 op mk2) ->
                case (calc mk1 m', calc mk2 m') of
                    (Just val1, Nothing) -> solve' mk2 (interpret1 op val val1)
                    (Nothing, Just val2) -> solve' mk1 (interpret2 op val val2)
                    _ -> Nothing -- cannot happen
            _ -> Nothing -- cannot happen

    interpret1 Add = (-)
    interpret1 Sub = flip (-)
    interpret1 Mul = div
    interpret1 Div = flip div
    
    interpret2 Add = (-)
    interpret2 Sub = (+)
    interpret2 Mul = div
    interpret2 Div = (*)


solve :: Text -> IO ()
solve = aoc parser part1 part2