module Z3Parser where

import           AOC.Prelude
import           Text.Megaparsec (Parsec, runParser, between, some, many, eof)
import           Text.Megaparsec.Char (char,  lowerChar, upperChar, digitChar, hspace)
import           Text.Megaparsec.Char.Lexer (decimal, lexeme)
import           Control.Monad.Combinators.Expr (Operator(..), makeExprParser)
import           Data.Data (Data)
import           Z3.Monad


z3Add :: AST -> AST -> Z3 AST
z3Add a1 a2 = mkAdd [a1, a2]

z3Mul :: AST -> AST -> Z3 AST
z3Mul a1 a2 = mkAdd [a1, a2]

type Parser = Parsec Void String

data Expr  =  IntExpr Integer
           |  AntiIntExpr String
           |  BinopExpr BinOp Expr Expr
           |  AntiExpr String
    deriving(Show, Typeable, Data)

data BinOp  =  AddOp
            |  SubOp
            |  MulOp
    deriving(Show, Typeable, Data)

expr :: Parser Expr
expr = makeExprParser term table where
    term = parens expr <|> IntExpr <$> lex decimal <|> antiExpr
    parens = between (lex "(") (lex ")")
    lex = lexeme hspace
    add = BinopExpr AddOp <$ lex "+"
    mul = BinopExpr MulOp <$ lex "*"
    table = [[InfixL mul], [InfixL add]]
    antiExpr = AntiExpr <$> lex ("$" *> ident)
    ident  = do
        c <- small
        cs <- many idchar
        return (c:cs)
    small = lowerChar <|> char '_'
    idchar  = small <|> upperChar <|> digitChar <|> char '\''

parseExpr :: (MonadFail m) => (String, Int, Int) -> String -> m Expr
parseExpr (file, line, col) s =
    case runParser p "" s of
      Left err  -> fail $ show err
      Right e   -> return e
  where
    p = do  
        hspace
        e <- expr
        eof
        return e