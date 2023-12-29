module AOC.Parser 
    ( module P
    , module C
    , module L
    , Parser
    , BinParser
    , signedDecimal
    , bitP
    , skipLine
    , format
    )
    where

import           AOC.Prelude
import           Text.Megaparsec as P (Parsec, anySingle, anySingleBut, between, choice, count, many, manyTill, 
                                        sepBy, sepBy1, sepEndBy1, optional, some, takeP, takeRest, takeWhileP, try)
import           Text.Megaparsec.Char as C (alphaNumChar, char, digitChar, eol, letterChar, space, hspace, hexDigitChar, lowerChar, numberChar, upperChar)
import           Text.Megaparsec.Char.Lexer as L (decimal, lexeme)
import qualified Language.Haskell.TH as TH
import           Language.Haskell.TH.Quote
import qualified AOC.List as L

type Parser = Parsec Void Text
type BinParser = P.Parsec Void [Bool]

signedDecimal :: Num a => Parser a
signedDecimal = L.decimal <|> C.char '-' *> (negate <$> L.decimal)

bitP :: Parser Bool
bitP = False <$ C.char '0' <|> True <$ C.char '1'

skipLine :: Parser ()
skipLine = void $ P.takeWhileP Nothing (/= '\n') *> C.eol

parseExpr :: MonadFail m => (String, Int, Int) -> String -> m [(String, Bool)]
parseExpr _ str = aux $ dropWhile (==' ') str
    where
    aux s =
        case span (/='{') s of
            (xs, []) -> pure [(xs, False)]
            ([], _:xs) -> aux2 xs
            (xs, _:ys) -> ((xs, False) :) <$> aux2 ys
    aux2 s = case span (/='}') s of
        (_, []) -> fail "no closing bracket"
        (xs, _:ys) -> ((xs, True) :) <$> aux ys

quoteExprExp :: String -> TH.ExpQ
quoteExprExp s =  do  loc <- TH.location
                      let pos =  (TH.loc_filename loc,
                                 fst (TH.loc_start loc),
                                 snd (TH.loc_start loc))
                      expr' <- parseExpr pos s
                      exprToExpQ expr'                    

{-
exprToExpQ :: [(String, Bool)] -> TH.ExpQ
exprToExpQ [] = [| pure () |]
exprToExpQ xs = case (ys, zs) of
    ([], [])               -> [| pure () |] 
    ((y,_):ys', [])        -> foldl' apply0 (str y) ys'
    ([], (z,_):zs')            -> foldl' applyN [| $tup <$> $(var z) |] zs'
    ((y,_):ys', (z,_):zs') -> let g = foldl' apply0 (str y) ys' in
                                if n == 1
                                    then foldl' apply1 [| $g *> $(var z) |] zs'
                                    else foldl' applyN [| $tup <$> ($g *> $(var z)) |] zs'
    where
    (ys, zs) = break snd xs
    tup = TH.conE (TH.tupleDataName n)
    n = L.count snd xs
    apply0 l (s,_) = [| $l *> $(var s) |]
    apply1 l (s, b') = if b' then [| $l  *> $(var s) |] else [| $l <* $(str s) |]
    applyN l (s, b') = if b' then [| $l <*> $(var s) |] else [| $l <* $(str s) |]
    var y = TH.varE (TH.mkName y)
    str y = TH.litE (TH.stringL y)
-}

exprToExpQ :: [(String, Bool)] -> TH.ExpQ
exprToExpQ [] = [| pure () |]
exprToExpQ xxs@((x, b) : xs) 
    | n == 0    = foldl' apply0 (str x) xs
    | n == 1    = foldl' apply1 (if b then var x else str x) xs
    | b         = foldl' applyN [| $tup <$> $(var x) |] xs
    | otherwise = foldl' applyN [| $tup <$  $(str x) |] xs
    where
    tup = TH.conE (TH.tupleDataName n)
    n = L.count snd xxs
    apply0 l (s,_) = [| $l *> $(var s) |]
    apply1 l (s, b') = if b' then [| $l  *> $(var s) |] else [| $l <* $(str s) |]
    applyN l (s, b') = if b' then [| $l <*> $(var s) |] else [| $l <* $(str s) |]
    var y = TH.varE (TH.mkName y)
    str y = TH.litE (TH.stringL y)

format :: QuasiQuoter
format = QuasiQuoter { quoteExp = quoteExprExp
                     , quotePat = \_ -> error "quotePat: not implemented"
                     , quoteType = \_ -> error "quoteType: not implemented"
                     , quoteDec = \_ -> error "quoteDec: not implemented"
                     }