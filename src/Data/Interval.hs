{-# LANGUAGE DeriveTraversable #-}
module Data.Interval
( Interval(..)
, size
, toUnit
, fromUnit
) where

import Control.Applicative (liftA2)

data Interval a = Interval
  { min_ :: !a
  , max_ :: !a
  }
  deriving (Eq, Foldable, Functor, Show, Traversable)

instance Applicative Interval where
  pure a = Interval a a
  Interval f1 f2 <*> Interval a1 a2 = Interval (f1 a1) (f2 a2)

instance Num a => Num (Interval a) where
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


size :: Num a => Interval a -> a
size (Interval min max) = max - min

toUnit, fromUnit :: Fractional a => Interval a -> a -> a
toUnit   i x = (x - min_ i) / size i
fromUnit i x =  x * size i  + min_ i
