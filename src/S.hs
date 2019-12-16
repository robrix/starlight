{-# LANGUAGE DeriveTraversable #-}
module S
( S(..)
, Var(..)
) where

data S a
  = Var a
  | S a :$ S a
  | Lam (S (Var a))
  deriving (Eq, Foldable, Functor, Ord, Show, Traversable)

infixr 9 :$


data Var a = B | F a
  deriving (Eq, Foldable, Functor, Ord, Show, Traversable)
