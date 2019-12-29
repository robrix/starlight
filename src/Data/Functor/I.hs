{-# LANGUAGE DeriveTraversable #-}
-- | An abbreviation of "Data.Functor.Identity".
module Data.Functor.I
( I(..)
) where

import Data.Coerce

newtype I a = I a
  deriving (Eq, Foldable, Functor, Ord, Show, Traversable)

instance Applicative I where
  pure = coerce
  (<*>) = coerce

instance Monad I where
  I a >>= f = f a
