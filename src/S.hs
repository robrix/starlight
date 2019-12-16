{-# LANGUAGE DeriveTraversable, LambdaCase #-}
module S
( S(..)
, Scope
, Var(..)
, lam
) where

import Control.Monad (ap)

data S a
  = Var a
  | S a :$ S a
  | Lam (Scope a)
  deriving (Eq, Foldable, Functor, Ord, Show, Traversable)

infixr 9 :$

instance Applicative S where
  pure = Var
  (<*>) = ap

instance Monad S where
  Var a  >>= f = f a
  g :$ a >>= f = (f =<< g) :$ (f =<< a)
  Lam b  >>= f = Lam . Scope $ unScope b >>= \case
    B   -> pure B
    F a -> F <$> f a


newtype Scope a = Scope { unScope :: S (Var a) }
  deriving (Eq, Foldable, Functor, Ord, Show, Traversable)

data Var a = B | F a
  deriving (Eq, Foldable, Functor, Ord, Show, Traversable)


lam :: Eq a => a -> S a -> S a
lam n = Lam . abstract1 n


abstract :: (a -> Var b) -> S a -> Scope b
abstract f = Scope . fmap f

abstract1 :: Eq a => a -> S a -> Scope a
abstract1 a = abstract (\b -> if a == b then B else F b)
