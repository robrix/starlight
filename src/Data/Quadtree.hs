{-# LANGUAGE DeriveTraversable #-}
module Data.Quadtree
( Q(..)
) where

data Q a
  = E
  | L a
  | Q (Q a) (Q a)
      (Q a) (Q a)
  deriving (Foldable, Functor, Eq, Ord, Show, Traversable)
