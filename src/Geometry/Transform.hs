module Geometry.Transform
( Transform(..)
, mkTranslation
, (>>>)
) where

import Control.Category
import Control.Lens ((&), (.~))
import Linear.Matrix
import Linear.V3

newtype Transform a b = Transform { getTransform :: M44 Float }
  deriving (Show)

instance Category Transform where
  id = Transform identity
  Transform a . Transform b = Transform (b !*! a)

mkTranslation :: V3 Float -> Transform a b
mkTranslation v = Transform (identity & translation .~ v)
