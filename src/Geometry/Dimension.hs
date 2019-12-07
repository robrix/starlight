{-# LANGUAGE KindSignatures #-}
module Geometry.Dimension
( Dimension(..)
) where

-- | Finite, fixed-length containers.
class Dimension (v :: * -> *) where
  dimension :: proxy v -> Int
