{-# LANGUAGE DeriveFunctor #-}
module Geometry.Triangle where

data Triangle v n = Triangle (v n) (v n) (v n)
  deriving Functor
