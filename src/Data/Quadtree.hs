module Data.Quadtree
( Q(..)
) where

data Q a
  = E
  | L a
  | Q (Q a) (Q a)
      (Q a) (Q a)
