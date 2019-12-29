{-# LANGUAGE DeriveTraversable #-}
module Data.Functor.Interval
( Interval(..)
) where

data Interval f a = Interval
  { min_ :: !(f a)
  , max_ :: !(f a)
  }
  deriving (Eq, Foldable, Functor, Show, Traversable)
