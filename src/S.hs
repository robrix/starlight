{-# LANGUAGE DeriveTraversable, LambdaCase #-}
module S
( S(..)
, Var(..)
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
