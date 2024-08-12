module Day11 (solve) where
import           AOC.Prelude
import           AOC (aocIO)
import           AOC.Parser (Parser, signedDecimal, sepBy1)
import           AOC.IntCode (Effect(..), newMachine, runEffect)
import           AOC.V2 (V2(..), origin, north, turnLeft, turnRight)
import           AOC.Draw (drawPicture)
import qualified Data.HashMap.Strict as Map

parser :: Parser [Int]
parser = signedDecimal `sepBy1` ","

simulate :: HashMap (V2 Int) Bool -> [Int] -> HashMap (V2 Int) Bool
simulate initialPaint pgm = go initialPaint (runEffect (newMachine pgm)) origin north where
    go paint effect pos dir = case effect of
        Halt _ -> paint
        Input f -> go paint (f currentColor) pos dir where
            currentColor = fromEnum $ Map.findWithDefault False pos paint
        Output color (Output turn eff') -> go paint' eff' pos' dir' where
            paint' = Map.insert pos (color==1) paint
            dir' = if turn == 0 then turnLeft dir else turnRight dir
            pos' = pos + dir' 
        _ -> error "part 1: illegal program"

part1 :: [Int] -> Int
part1 = Map.size . simulate Map.empty

part2 :: [Int] -> IO Int
part2 pgm = putStr drawing $> 0 where
    drawing = drawPicture 
            . fmap (bool ' ' '#')
            . Map.mapKeys (\(V2 x y) -> V2 y x)
            $ simulate (Map.singleton origin True) pgm

solve :: Text -> IO ()
solve = aocIO parser (pure . part1) part2