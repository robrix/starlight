{-# LANGUAGE DeriveTraversable #-}
module S
( S(..)
) where

data S a
  = Var a
  | S a :$ S a
  | Lam (S (Maybe a))
  deriving (Eq, Foldable, Functor, Ord, Show, Traversable)

infixr 9 :$
