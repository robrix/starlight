{-# LANGUAGE KindSignatures #-}
module Geometry.Dimension
( Dimension(..)
) where

class Dimension (v :: * -> *) where
  dimension :: proxy v -> Int
