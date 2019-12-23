{-# LANGUAGE DeriveTraversable #-}
module Data.Interval
( Interval(..)
) where

data Interval a = Interval
  { from :: !a
  , to   :: !a
  }
  deriving (Eq, Foldable, Functor, Show, Traversable)
