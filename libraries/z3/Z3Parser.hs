{-# LANGUAGE DeriveAnyClass #-}

module Z3Parser where

import           AOC.Prelude
import           Text.Megaparsec (Parsec, runParser, between, some, many, eof)
import           Text.Megaparsec.Char (char,  lowerChar, upperChar, digitChar, hspace)
import           Text.Megaparsec.Char.Lexer (decimal, lexeme)
import           Control.Monad.Combinators.Expr (Operator(..), makeExprParser)

data Expr  = IntExpr Integer
           | BinopExpr BinOp Expr Expr
           | AntiExpr String
    deriving(Show, Typeable)

data BinOp  =  AddOp
            -- |  SubOp
            |  MulOp
            | EqOp
            | GeOp
            | LeOp
    deriving(Show, Typeable)

expr :: Parsec Void String Expr
expr = makeExprParser term table where
    term = parens expr <|> IntExpr <$> lex decimal <|> antiExpr
    parens = between (lex "(") (lex ")")
    lex = lexeme hspace
    add = BinopExpr AddOp <$ lex "+"
    mul = BinopExpr MulOp <$ lex "*"
    eq = BinopExpr EqOp <$ lex "=="
    le = BinopExpr LeOp <$ lex "<="
    ge = BinopExpr GeOp <$ lex ">="
    table = [[InfixL mul], [InfixL add], [InfixL eq, InfixL ge, InfixL le]]
    antiExpr = AntiExpr <$> lex ident
    ident  = do
        c <- small
        cs <- many idchar
        return (c:cs)
    small = lowerChar <|> char '_'
    idchar  = small <|> upperChar <|> digitChar <|> char '\''

parseExpr :: (MonadFail m) => (String, Int, Int) -> String -> m Expr
parseExpr _ s =
    case runParser p "" s of
      Left err  -> fail $ show err
      Right e   -> return e
  where
    p = do
        hspace
        e <- expr
        eof
        return e

{-
eval :: Expr -> Z3 AST
eval (IntExpr n) = mkInteger n
eval (Z3Ast ast) = pure ast
eval (BinopExpr op e1 e2) = evalOp op (eval e1) (eval e2)
    where
    evalOp AddOp = z3Add
    evalOp MulOp = z3Mul
eval _ = error "cannot happen"
-}