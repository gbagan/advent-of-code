module AOC.V3 where

import AOC.Prelude hiding (tail)
import Data.Foldable1 (Foldable1, foldMap1)
import Data.List (tail)
import Lens.Micro (Lens', lens)

data V3 a = V3 !a !a !a deriving (Eq,Generic,Ord,Show)

instance Functor V3 where
    fmap f (V3 a b c) = V3 (f a) (f b) (f c)
    {-# INLINE fmap #-}
    a <$ _ = V3 a a a
    {-# INLINE (<$) #-}

instance Applicative V3 where
  pure a = V3 a a a
  {-# INLINE pure #-}
  V3 a b c <*> V3 d e f = V3 (a d) (b e) (c f)
  {-# INLINE (<*>) #-}

instance Foldable V3 where
    foldMap f (V3 a b c) = f a `mappend` f b `mappend` f c
    {-# INLINE foldMap #-}
    foldMap' f (V3 a b c) = f a `mappend` f b `mappend` f c
    {-# INLINE foldMap' #-}
    null _ = False
    length _ = 3

instance Traversable V3 where
    traverse f (V3 a b c) = V3 <$> f a <*> f b <*> f c
    {-# INLINE traverse #-}

instance Foldable1 V3 where
    foldMap1 f (V3 a b c) = f a <> f b <> f c
    {-# INLINE foldMap1 #-}

instance Hashable a => Hashable (V3 a)

instance Num a => Num (V3 a) where
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

surrounding :: Integral a => V3 a -> [V3 a]
surrounding (V3 x y z) = tail (V3 <$> [x, x-1, x+1] <*> [y, y-1, y+1] <*> [z, z-1, z+1])

_x :: Lens' (V3 a) a
_x = lens (\(V3 x _ _) -> x) (\(V3 _ y z) x -> V3 x y z)

_y :: Lens' (V3 a) a
_y = lens (\(V3 _ y _) -> y) (\(V3 x _ z) y -> V3 x y z)

_z :: Lens' (V3 a) a
_z = lens (\(V3 _ _ z) -> z) (\(V3 x y _) z -> V3 x y z)

manhattan :: Num a => V3 a -> V3 a -> a
manhattan (V3 x1 y1 z1) (V3 x2 y2 z2) = abs (x1 - x2) + abs (y1 - y2) + abs (z1 - z2)
{-# INLINE manhattan #-}

origin :: Num a => V3 a
origin = V3 0 0 0
{-# INLINE origin #-}
