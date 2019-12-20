{-# LANGUAGE DeriveTraversable, GeneralizedNewtypeDeriving #-}
module Unit.Length
( Metres(..)
, fromKilometres
, Kilometres(..)
) where

import GL.Uniform

newtype Metres a = Metres { getMetres :: a }
  deriving (Eq, Foldable, Floating, Fractional, Functor, Num, Ord, Real, RealFloat, RealFrac, Show, Traversable, Uniform)

fromKilometres :: Num a => Kilometres a -> Metres a
fromKilometres (Kilometres k) = Metres (k * 1000)


newtype Kilometres a = Kilometres { getKilometres :: a }
  deriving (Eq, Foldable, Floating, Fractional, Functor, Num, Ord, Real, RealFloat, RealFrac, Show, Traversable, Uniform)
