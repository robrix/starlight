{-# LANGUAGE DeriveTraversable #-}
module Data.Functor.Interval
( Interval(..)
) where

import Control.Applicative (liftA2)

data Interval f a = Interval
  { min_ :: !(f a)
  , max_ :: !(f a)
  }
  deriving (Eq, Foldable, Functor, Show, Traversable)

instance Applicative f => Applicative (Interval f) where
  pure a = Interval (pure a) (pure a)
  Interval f1 f2 <*> Interval a1 a2 = Interval (f1 <*> a1) (f2 <*> a2)

instance (Applicative f, Num a) => Num (Interval f a) where
  (+) = liftA2 (+)
  {-# INLINE (+) #-}
  (*) = liftA2 (*)
  {-# INLINE (*) #-}
  (-) = liftA2 (-)
  {-# INLINE (-) #-}
  abs = fmap abs
  {-# INLINE abs #-}
  signum = fmap signum
  {-# INLINE signum #-}
  negate = fmap negate
  {-# INLINE negate #-}
  fromInteger = pure . fromInteger
  {-# INLINE fromInteger #-}

instance (Applicative f, Fractional a) => Fractional (Interval f a) where
  recip = fmap recip
  {-# INLINE recip #-}
  (/) = liftA2 (/)
  {-# INLINE (/) #-}
  fromRational = pure . fromRational
  {-# INLINE fromRational #-}
