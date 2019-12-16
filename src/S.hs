{-# LANGUAGE DeriveTraversable #-}
module S
( S(..)
, Scope
, lam
, close
) where

import Control.Effect.Empty
import Control.Monad (ap)
import Data.Void

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
  Lam b  >>= f = Lam . Scope $ unScope b >>= maybe (pure Nothing) (fmap Just . f)


newtype Scope a = Scope { unScope :: S (Maybe a) }
  deriving (Eq, Foldable, Functor, Ord, Show, Traversable)


lam :: Eq a => a -> S a -> S a
lam n = Lam . abstract1 n


close :: Has Empty sig m => S a -> m (S Void)
close = traverse (const empty)


abstract :: (a -> Maybe b) -> S a -> Scope b
abstract f = Scope . fmap f

abstract1 :: Eq a => a -> S a -> Scope a
abstract1 a = abstract (\b -> if a == b then Nothing else Just b)
