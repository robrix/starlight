{-# LANGUAGE DeriveTraversable #-}
module S
( S(..)
, Var(..)
) where

data S a
  = Var a
  | S a :$ S a
  | Lam (Scope a)
  deriving (Eq, Foldable, Functor, Ord, Show, Traversable)

infixr 9 :$

newtype Scope a = Scope { unScope :: S (Var a) }
  deriving (Eq, Foldable, Functor, Ord, Show, Traversable)

data Var a = B | F a
  deriving (Eq, Foldable, Functor, Ord, Show, Traversable)
