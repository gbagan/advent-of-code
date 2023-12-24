module Z3Quote where

import AOC.Prelude
import Data.Data
import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Quote
import Z3Parser

{-
quoteExprExp :: String -> TH.ExpQ
quoteExprExp s =  do  loc <- TH.location
                      let pos =  (TH.loc_filename loc,
                                 fst (TH.loc_start loc),
                                 snd (TH.loc_start loc))
                      expr <- parseExpr pos s
                      dataToExpQ (const Nothing `extQ` antiExprExp) expr

antiExprExp :: Expr -> Maybe (TH.Q TH.Exp)
antiExprExp  (AntiExpr v)     = Just $ TH.varE (TH.mkName v)
antiExprExp  _                = Nothing

quoteExprPat :: String -> TH.PatQ
quoteExprPat s =  do  loc <- TH.location
                      let pos =  (TH.loc_filename loc,
                                 fst (TH.loc_start loc),
                                 snd (TH.loc_start loc))
                      expr <- parseExpr pos s
                      dataToPatQ (const Nothing `extQ` antiExprPat) expr

antiExprPat :: Expr -> Maybe (TH.Q TH.Pat)
antiExprPat  (AntiExpr v)     = Just $ TH.varP  (TH.mkName v)
antiExprPat  _                = Nothing
-}