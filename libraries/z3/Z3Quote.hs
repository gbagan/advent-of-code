module Z3Quote (expr) where

import AOC.Prelude
import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Quote
import Z3Parser

quoteExprExp :: String -> TH.ExpQ
quoteExprExp s =  do  loc <- TH.location
                      let pos =  (TH.loc_filename loc,
                                 fst (TH.loc_start loc),
                                 snd (TH.loc_start loc))
                      expr' <- parseExpr pos s
                      exprToExpQ expr'                    

exprToExpQ :: Expr -> TH.ExpQ
exprToExpQ (IntExpr n) = TH.appE 
                            (TH.varE (TH.mkName "z3int"))
                            (TH.litE (TH.integerL n)) 
exprToExpQ (AntiExpr v) = TH.varE (TH.mkName v)
exprToExpQ (BinopExpr op e1 e2) = TH.appE (TH.appE 
                                    (opExprToExpQ op)
                                    (exprToExpQ e1)
                                    ) (exprToExpQ e2)

opExprToExpQ :: BinOp -> TH.ExpQ
opExprToExpQ AddOp = TH.varE (TH.mkName "z3add")
opExprToExpQ MulOp = TH.varE (TH.mkName "z3mul")
opExprToExpQ EqOp = TH.varE (TH.mkName "z3eq")
opExprToExpQ LeOp = TH.varE (TH.mkName "z3le")
opExprToExpQ GeOp = TH.varE (TH.mkName "z3ge")

expr  :: QuasiQuoter
expr  =  QuasiQuoter { quoteExp = quoteExprExp
                     , quotePat = \_ -> error "quotePat: not implemented"
                     , quoteType = \_ -> error "quoteType: not implemented"
                     , quoteDec = \_ -> error "quoteDec: not implemented"
                     }