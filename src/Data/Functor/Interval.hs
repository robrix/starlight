{-# LANGUAGE DeriveTraversable #-}
module Data.Functor.Interval
( Interval(..)
) where

data Interval f a = Interval
  { min_ :: !(f a)
  , max_ :: !(f a)
  }
  deriving (Eq, Foldable, Functor, Show, Traversable)

instance Applicative f => Applicative (Interval f) where
  pure a = Interval (pure a) (pure a)
  Interval f1 f2 <*> Interval a1 a2 = Interval (f1 <*> a1) (f2 <*> a2)
