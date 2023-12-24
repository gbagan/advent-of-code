module Z3Helpers where

import           AOC.Prelude
import           Z3.Monad (Z3, AST, mkInteger, mkAdd, mkMul, mkEq, mkGe, mkLe, withModel, evalInt)

class ToZ3 a where
    toZ3 :: a -> Z3 AST

instance ToZ3 (Z3 AST) where
    toZ3 = id

instance ToZ3 AST where
    toZ3 = pure

instance ToZ3 Int where
    toZ3 = mkInteger . fromIntegral

instance ToZ3 Integer where
    toZ3 = mkInteger

(+&) :: (ToZ3 a, ToZ3 b) => a -> b -> Z3 AST
a +& b = do
    x <- toZ3 a
    y <- toZ3 b
    mkAdd [x, y]
infixl 6 +&

(*&) :: (ToZ3 a, ToZ3 b) => a -> b -> Z3 AST
a *& b = do
    x <- toZ3 a
    y <- toZ3 b
    mkMul [x, y]
infixl 7 *&

(==&) :: (ToZ3 a, ToZ3 b) => a -> b -> Z3 AST
a ==& b = do
    x <- toZ3 a
    y <- toZ3 b
    mkEq x y
infixl 5 ==&

(>=&) :: (ToZ3 a, ToZ3 b) => a -> b -> Z3 AST
a >=& b = do
    x <- toZ3 a
    y <- toZ3 b
    mkGe x y
infixl 5 >=&

(<=&) :: (ToZ3 a, ToZ3 b) => a -> b -> Z3 AST
a <=& b = do
    x <- toZ3 a
    y <- toZ3 b
    mkLe x y
infixl 5 <=&

getIntResults :: [AST] -> Z3 (Maybe [Integer])
getIntResults asts =
    fmap snd . withModel $ \m ->
        catMaybes <$> mapM (evalInt m) asts



