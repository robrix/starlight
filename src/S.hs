{-# LANGUAGE DataKinds, DeriveTraversable, EmptyCase, GADTs, LambdaCase, StandaloneDeriving #-}
module S
( S(..)
, Scope
, lam
, unlam
, close
, Nat(..)
, Fin(..)
, absurd
) where

import Control.Effect.Empty
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
  Lam b  >>= f = Lam . Scope $ unScope b >>= maybe (pure Nothing) (fmap Just . f)


newtype Scope a = Scope { unScope :: S (Maybe a) }
  deriving (Eq, Foldable, Functor, Ord, Show, Traversable)


lam :: Eq a => a -> S a -> S a
lam n = Lam . abstract n

unlam :: Has Empty sig m => a -> S a -> m (a, S a)
unlam a = \case
  Lam b -> pure (a, instantiate (pure a) b)
  _     -> empty


close :: Has Empty sig m => S a -> m (S (Fin 'Z))
close = traverse (const empty)


abstract :: Eq a => a -> S a -> Scope a
abstract a = Scope . fmap (\b -> if a == b then Nothing else Just b)


instantiate :: S a -> Scope a -> S a
instantiate k e = unScope e >>= maybe k pure


data Nat = Z | S Nat

data Fin n where
  FZ :: Fin ('S n)
  FS :: Fin n -> Fin ('S n)

deriving instance Eq   (Fin n)
deriving instance Ord  (Fin n)
deriving instance Show (Fin n)

absurd :: Fin 'Z -> a
absurd f = case f of {}
