{-# LANGUAGE DeriveFunctor #-}
module Geometry.Triangle where

import Linear.V2 as Linear

data Triangle n = Triangle
  {-# UNPACK #-} !(V2 n)
  {-# UNPACK #-} !(V2 n)
  {-# UNPACK #-} !(V2 n)
  deriving Functor
