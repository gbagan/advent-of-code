module AOC.V2 where

import Relude
import Data.Foldable1 (Foldable1, foldMap1)

data V2 a = V2 !a !a deriving (Eq,Ord,Show)

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