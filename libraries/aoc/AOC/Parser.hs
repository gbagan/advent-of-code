module AOC.Parser 
    ( module P
    , module C
    , module L
    , Parser
    , BinParser
    , signedDecimal
    , bitP
    , skipLine
    , scanf
    )
    where

import           AOC.Prelude hiding (head)
import           Data.List (head)
import           Text.Megaparsec as P (Parsec, anySingle, anySingleBut, between, choice, count, many, manyTill, 
                                        sepBy, sepBy1, sepEndBy1, optional, some, takeP, takeRest, takeWhileP, takeWhile1P, try)
import           Text.Megaparsec.Char as C (alphaNumChar, char, digitChar, eol, letterChar, space, hspace, hexDigitChar, lowerChar, numberChar, upperChar)
import           Text.Megaparsec.Char.Lexer as L (decimal, lexeme)
import qualified Language.Haskell.TH as TH
import           Language.Haskell.TH.Quote
import           Data.Char (isSpace, isUpper)
import qualified AOC.List as L

type Parser = Parsec Void Text
type BinParser = P.Parsec Void [Bool]

signedDecimal :: Num a => Parser a
signedDecimal = L.decimal <|> C.char '-' *> (negate <$> L.decimal)

bitP :: Parser Bool
bitP = False <$ C.char '0' <|> True <$ C.char '1'

skipLine :: Parser ()
skipLine = void $ P.takeWhileP Nothing (/= '\n') *> C.eol

parseExpr :: MonadFail m => (String, Int, Int) -> String -> m (Maybe String, [(String, Bool)])
parseExpr _ str =
    case str' of
        ('$':xs) -> 
            let (ys,zs) = break isSpace xs in
            (Just ys,) <$> aux (dropWhile (==' ') zs)
        _ -> (Nothing,) <$> aux str'
    where
    str' = dropWhile (==' ') str
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

exprToExpQ :: (Maybe String, [(String, Bool)]) -> TH.ExpQ
exprToExpQ = \case 
    (_, []) -> [| pure () |]
    (Nothing, xxs@((x, b) : xs))
        | n == 0    -> foldl' apply0 (str x) xs
        | n == 1    -> foldl' apply1 (if b then var x else str x) xs
        | b         -> foldl' applyN [| $tup <$> $(var x) |] xs
        | otherwise -> foldl' applyN [| $tup <$  $(str x) |] xs
        where
        tup = TH.conE (TH.tupleDataName n)
        n = L.count snd xxs
    (Just fn, (x, b) : xs)
        | b         -> foldl' applyN [| $(var fn) <$> $(var x) |] xs
        | otherwise -> foldl' applyN [| $(var fn) <$  $(str x) |] xs
    where
    apply0 l (s,_) = [| $l *> $(var s) |]
    apply1 l (s, b') = if b' then [| $l  *> $(var s) |] else [| $l <* $(str s) |]
    applyN l (s, b') = if b' then [| $l <*> $(var s) |] else [| $l <* $(str s) |]
    var y = if isUpper (head y) then TH.conE (TH.mkName y) else TH.varE (TH.mkName y)
    str y = TH.litE (TH.stringL y)

scanf :: QuasiQuoter
scanf = QuasiQuoter { quoteExp = quoteExprExp
                     , quotePat = \_ -> error "quotePat: not implemented"
                     , quoteType = \_ -> error "quoteType: not implemented"
                     , quoteDec = \_ -> error "quoteDec: not implemented"
                     }