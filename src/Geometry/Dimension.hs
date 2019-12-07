{-# LANGUAGE KindSignatures #-}
module Geometry.Dimension
( Dimension(..)
) where

import Linear.V0

-- | Finite, fixed-length containers.
class Dimension (v :: * -> *) where
  dimension :: proxy v -> Int

instance Dimension V0 where
  dimension _ = 0
