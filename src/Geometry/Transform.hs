module Geometry.Transform
( Transform(..)
, (>>>)
) where

import Control.Category
import Linear.Matrix

newtype Transform a b = Transform { getTransform :: M44 Float }
  deriving (Show)

instance Category Transform where
  id = Transform identity
  Transform a . Transform b = Transform (b !*! a)
