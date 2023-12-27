module AOC.Parser 
    ( module P
    , module C
    , module L
    , Parser
    , BinParser
    , signedDecimal
    , bitP
    , skipLine
    --, format
    )
    where

import           AOC.Prelude
import           Text.Megaparsec as P (Parsec, anySingle, anySingleBut, between, choice, count, many, manyTill, 
                                        sepBy, sepBy1, sepEndBy1, optional, some, takeP, takeRest, takeWhileP, try)
import           Text.Megaparsec.Char as C (alphaNumChar, char, digitChar, eol, letterChar, space, hspace, hexDigitChar, lowerChar, numberChar, upperChar)
import           Text.Megaparsec.Char.Lexer as L (decimal, lexeme)
import qualified Language.Haskell.TH as TH
import           Language.Haskell.TH.Quote
import AOC.Debug (spy)

type Parser = Parsec Void Text
type BinParser = P.Parsec Void [Bool]

signedDecimal :: Num a => Parser a
signedDecimal = L.decimal <|> C.char '-' *> (negate <$> L.decimal)

bitP :: Parser Bool
bitP = False <$ C.char '0' <|> True <$ C.char '1'

skipLine :: Parser ()
skipLine = void $ P.takeWhileP Nothing (/= '\n') *> C.eol

parseExpr :: MonadFail m => (String, Int, Int) -> String -> m [Either String String]
parseExpr _ str = aux $ dropWhile (==' ') str
    where
    aux s =
        case span (/='{') s of
            (xs, []) -> pure [Left xs]
            ([], (_:xs)) -> aux2 xs
            (xs, (_:ys)) -> (Left xs :) <$> aux2 ys
    aux2 s = case span (/='}') s of
        (_, []) -> fail "no closing bracket"
        (xs, (_:ys)) -> (Right xs :) <$> aux ys

{-
quoteExprExp :: String -> TH.ExpQ
quoteExprExp s =  do  loc <- TH.location
                      let pos =  (TH.loc_filename loc,
                                 fst (TH.loc_start loc),
                                 snd (TH.loc_start loc))
                      expr' <- parseExpr pos s
                      name <- TH.newName "x"
                      exprToExpQ expr'                    

exprToExpQ :: [Either String String] -> TH.ExpQ
exprToExpQ [] = error "format: empty expression"
exprToExpQ [Left x] = TH.litE (TH.stringL x)
exprToExpQ [Right x] = TH.varE (TH.mkName x)
exprToExpQ (Left x : xs) =
    TH.appE 
        (TH.appE
            (TH.varE (TH.mkName "*>"))
            (TH.litE (TH.stringL x))
        ) (exprToExpQ xs)
exprToExpQ [Right x, Left y] = 
    TH.appE
        (TH.appE 
            (TH.varE (TH.mkName "<*"))
            (TH.varE (TH.mkName x))
        )
        (TH.litE (TH.stringL y))
exprToExpQ (Right x : xs) =
    TH.appE
        (TH.appE 
            (TH.varE (TH.mkName "<*>"))
            (TH.varE (TH.mkName x))
        )
        (exprToExpQ xs)

format :: QuasiQuoter
format = QuasiQuoter { quoteExp = quoteExprExp
                     , quotePat = \_ -> error "quotePat: not implemented"
                     , quoteType = \_ -> error "quoteType: not implemented"
                     , quoteDec = \_ -> error "quoteDec: not implemented"
                     }
-}