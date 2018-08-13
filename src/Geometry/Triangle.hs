{-# LANGUAGE DeriveFunctor #-}
module Geometry.Triangle where

import Linear.V2 as Linear

data Triangle n = Triangle (V2 n) (V2 n) (V2 n)
  deriving Functor
