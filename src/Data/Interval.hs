{-# LANGUAGE DeriveTraversable #-}
module Data.Interval
( Interval(..)
) where

data Interval a = Interval
  { from :: !a
  , to   :: !a
  }
  deriving (Eq, Foldable, Functor, Show, Traversable)

instance Applicative Interval where
  pure a = Interval a a
  Interval f1 f2 <*> Interval a1 a2 = Interval (f1 a1) (f2 a2)
