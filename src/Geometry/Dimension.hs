{-# LANGUAGE KindSignatures #-}
module Geometry.Dimension
( Dimension(..)
) where

import Linear.V0
import Linear.V1
import Linear.V2
import Linear.V3

-- | Finite, fixed-length containers.
class Dimension (v :: * -> *) where
  dimension :: proxy v -> Int

instance Dimension V0 where
  dimension _ = 0

instance Dimension V1 where
  dimension _ = 1

instance Dimension V2 where
  dimension _ = 2

instance Dimension V3 where
  dimension _ = 3
