module Day24 (solve) where
import           RIO
import           RIO.List.Partial ((!!))
import           Text.Megaparsec (sepEndBy1)
import qualified Text.Megaparsec.Char as P
import           Util (Parser, aocTemplate, signedInteger)

data Var = W | X | Y | Z deriving (Eq, Ord)
data Val = Var Var | Int Int
data Instr = Input Var | Add Var Val |  Mul Var Val | Div Var Val | Mod Var Val | Eql Var Val

parser :: Parser [Instr]
parser = instr `sepEndBy1` P.eol where
    instr = input <|> binop <*> var <* P.char ' ' <*> val
    input = P.string "inp " $> Input <*> var
    binop = P.string "add " $> Add
        <|> P.string "mul " $> Mul
        <|> P.string "div " $> Div
        <|> P.string "mod " $> Mod
        <|> P.string "eql " $> Eql 
    var = P.char 'w' $> W <|> P.char 'x' $> X <|> P.char 'y' $> Y <|> P.char 'z' $> Z
    val = Var <$> var <|> Int <$> signedInteger

precomp :: [Instr] -> (Int, Int)
precomp instrs = fst $ foldl' go ((p, q), []) [0..13] where
    go ((p', q'), stack) i =
        let Add _ (Int a) = instrs !! (18*i+5)
            Add _ (Int b) = instrs !! (18*i+15)
        in if a > 0
            then ((p', q'), (i, b) : stack)
            else
                let (j, b) : stack' = stack
                    p'' = p' - abs ((a+b)*10^(13 - if a > -b then j else i))
                    q'' = q' + abs ((a+b)*10^(13 - if a < -b then j else i))
                in ((p'', q''), stack')
    p = 99999999999999
    q = 11111111111111

solve :: (HasLogFunc env) => Text -> RIO env ()
solve = aocTemplate parser (pure . precomp) (pure . fst) (pure . snd)