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

z3int :: Integer -> Z3 AST
z3int = mkInteger

z3add :: (ToZ3 a, ToZ3 b) => a -> b -> Z3 AST
z3add a b = do
    x <- toZ3 a
    y <- toZ3 b
    mkAdd [x, y]

z3mul :: (ToZ3 a, ToZ3 b) => a -> b -> Z3 AST
z3mul a b = do
    x <- toZ3 a
    y <- toZ3 b
    mkMul [x, y]

z3eq :: (ToZ3 a, ToZ3 b) => a -> b -> Z3 AST
z3eq a b = do
    x <- toZ3 a
    y <- toZ3 b
    mkEq x y

z3ge :: (ToZ3 a, ToZ3 b) => a -> b -> Z3 AST
z3ge a b = do
    x <- toZ3 a
    y <- toZ3 b
    mkGe x y

z3le :: (ToZ3 a, ToZ3 b) => a -> b -> Z3 AST
z3le a b = do
    x <- toZ3 a
    y <- toZ3 b
    mkGe x y

getIntResults :: [AST] -> Z3 (Maybe [Integer])
getIntResults asts =
    fmap snd . withModel $ \m ->
        catMaybes <$> mapM (evalInt m) asts
