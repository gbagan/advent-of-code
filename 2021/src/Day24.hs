module Day24 (solve) where
import           RIO
import           RIO.List.Partial ((!!))
import           Text.Megaparsec (sepEndBy1)
import           Text.Megaparsec.Char (char, eol, string)
import           Util (Parser, aoc', signedInteger)

data Var = W | X | Y | Z deriving (Eq, Ord)
data Val = Var Var | Int Int
data Instr = Input Var | Add Var Val |  Mul Var Val | Div Var Val | Mod Var Val | Eql Var Val

parser :: Parser [Instr]
parser = instr `sepEndBy1` eol where
    instr = input <|> binop <*> var <* char ' ' <*> val
    input = string "inp " $> Input <*> var
    binop = string "add " $> Add
        <|> string "mul " $> Mul
        <|> string "div " $> Div
        <|> string "mod " $> Mod
        <|> string "eql " $> Eql 
    var = char 'w' $> W <|> char 'x' $> X <|> char 'y' $> Y <|> char 'z' $> Z
    val = Var <$> var <|> Int <$> signedInteger

precomp :: [Instr] -> (Int, Int)
precomp instrs = fst $ foldl' go ((p, q), []) [0..13] where
    go ((p', q'), stack) i =
        let Add _ (Int a) = instrs !! (18*i+5)
            Add _ (Int b) = instrs !! (18*i+15)
        in if a > 0
            then ((p', q'), (i, b) : stack)
            else
                let (j, c) : stack' = stack
                    p'' = p' - abs ((a+c)*10^(13 - if a > -c then j else i))
                    q'' = q' + abs ((a+c)*10^(13 - if a < -c then j else i))
                in ((p'', q''), stack')
    p = 99999999999999
    q = 11111111111111

solve :: (HasLogFunc env) => Text -> RIO env ()
solve = aoc' parser (pure . precomp) fst snd