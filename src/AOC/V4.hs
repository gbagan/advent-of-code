module AOC.V4 where

import AOC.Prelude hiding (tail)
import Data.Foldable1 (Foldable1, foldMap1)
import Data.List (tail)
import Lens.Micro (Lens', lens)

data V4 a = V4 !a !a !a !a deriving (Eq,Ord,Show)

instance Functor V4 where
    fmap f (V4 a b c d) = V4 (f a) (f b) (f c) (f d)
    {-# INLINE fmap #-}
    a <$ _ = V4 a a a a
    {-# INLINE (<$) #-}

instance Applicative V4 where
  pure a = V4 a a a a
  {-# INLINE pure #-}
  V4 a b c d <*> V4 e f g h = V4 (a e) (b f) (c g) (d h)
  {-# INLINE (<*>) #-}

instance Foldable V4 where
    foldMap f (V4 a b c d) = f a `mappend` f b `mappend` f c `mappend` f d
    {-# INLINE foldMap #-}
    foldMap' f (V4 a b c d) = f a `mappend` f b `mappend` f c `mappend` f d
    {-# INLINE foldMap' #-}
    null _ = False
    length _ = 3

instance Traversable V4 where
    traverse f (V4 a b c d) = V4 <$> f a <*> f b <*> f c <*> f d
    {-# INLINE traverse #-}

instance Foldable1 V4 where
    foldMap1 f (V4 a b c d) = f a <> f b <> f c <> f d
    {-# INLINE foldMap1 #-}

instance Hashable a => Hashable (V4 a) where
    hashWithSalt s (V4 a b c d) = s `hashWithSalt` a `hashWithSalt` b `hashWithSalt` c `hashWithSalt` d
    {-# INLINE hashWithSalt #-}

instance Num a => Num (V4 a) where
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

_x :: Lens' (V4 a) a
_x = lens (\(V4 x _ _ _) -> x) (\(V4 _ y z t) x -> V4 x y z t)

_y :: Lens' (V4 a) a
_y = lens (\(V4 _ y _ _) -> y) (\(V4 x _ z t) y -> V4 x y z t)

_z :: Lens' (V4 a) a
_z = lens (\(V4 _ _ z _) -> z) (\(V4 x y _ t) z -> V4 x y z t)

_t :: Lens' (V4 a) a
_t = lens (\(V4 _ _ _ t) -> t) (\(V4 x y z _) t -> V4 x y z t)

surrounding :: Integral a => V4 a -> [V4 a]
surrounding p = [p + V4 dx dy dz dt | [dx,dy,dz,dt] <- tail (replicateM 4 [0,-1,1])]