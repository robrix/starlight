{-# LANGUAGE DeriveTraversable #-}
-- | An abbreviation of "Data.Functor.Identity".
module Data.Functor.I
( I(..)
) where

newtype I a = I a
  deriving (Eq, Foldable, Functor, Ord, Show, Traversable)
