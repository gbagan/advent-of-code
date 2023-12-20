module AOC.V3 where

import AOC.Prelude hiding (tail)
import Data.Foldable1 (Foldable1, foldMap1)
import Data.List (tail)

data V3 a = V3 !a !a !a deriving (Eq,Ord,Show)

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

instance Hashable a => Hashable (V3 a) where
    hashWithSalt s (V3 a b c) = s `hashWithSalt` a `hashWithSalt` b `hashWithSalt` c
    {-# INLINE hashWithSalt #-}

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
surrounding p = [p + V3 dx dy dz | [dx,dy,dz] <- tail (replicateM 3 [0,-1,1])]