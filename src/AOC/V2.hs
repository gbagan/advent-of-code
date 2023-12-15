module AOC.V2 where

import Relude
import Data.Foldable1 (Foldable1, foldMap1)
import           Data.Massiv.Array (Ix2(..))

data V2 a = V2 { _x :: !a, _y :: !a } deriving (Eq,Ord,Show)

instance Functor V2 where
    fmap f (V2 a b) = V2 (f a) (f b)
    {-# INLINE fmap #-}
    a <$ _ = V2 a a
    {-# INLINE (<$) #-}

instance Applicative V2 where
  pure a = V2 a a
  {-# INLINE pure #-}
  V2 a b <*> V2 d e = V2 (a d) (b e)
  {-# INLINE (<*>) #-}

instance Foldable V2 where
    foldMap f (V2 a b) = f a `mappend` f b
    {-# INLINE foldMap #-}
    foldMap' f (V2 a b) = f a `mappend` f b
    {-# INLINE foldMap' #-}
    null _ = False
    length _ = 2

instance Traversable V2 where
    traverse f (V2 a b) = V2 <$> f a <*> f b
    {-# INLINE traverse #-}

instance Foldable1 V2 where
    foldMap1 f (V2 a b) = f a <> f b
    {-# INLINE foldMap1 #-}

instance Hashable a => Hashable (V2 a) where
    hashWithSalt s (V2 a b) = s `hashWithSalt` a `hashWithSalt` b
    {-# INLINE hashWithSalt #-}

instance Num a => Num (V2 a) where
    (+) = liftA2 (+)
    {-# INLINE (+) #-}
    (-) = liftA2 (-)
    {-# INLINE (-) #-}
    (*) = liftA2 (*)
    {-# INLINE (*) #-}
    negate = fmap negate
    {-# INLINE negate #-}
    abs = fmap abs
    {-# INLINE abs #-}
    signum = fmap signum
    {-# INLINE signum #-}
    fromInteger = pure . fromInteger
    {-# INLINE fromInteger #-}

perp :: Num a => V2 a -> V2 a
perp (V2 a b) = V2 (negate b) a
{-# INLINE perp #-}

manhattan :: Num a => V2 a -> V2 a -> a
manhattan (V2 x1 y1) (V2 x2 y2) = abs (x1 - x2) + abs (y1 - y2)

adjacent :: Integral a => V2 a -> [V2 a]
adjacent (V2 x y) = [V2 (x-1) y, V2 (x+1) y, V2 x (y-1), V2 x (y+1)]

surrounding :: Integral a => V2 a -> [V2 a]
surrounding (V2 x y) = adjacent (V2 x y) ++ [ V2 (x-1) (y-1)
                                            , V2 (x+1) (y-1)
                                            , V2 (x-1) (y+1)
                                            , V2 (x+1) (y+1)
                                            ]

toIx2 :: V2 Int -> Ix2
toIx2 (V2 x y) = Ix2 x y