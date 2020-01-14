module Geometry.Transform
( Transform(..)
, mkTranslation
, mkRotation
, apply
, (>>>)
) where

import Control.Category
import Control.Lens ((&), (.~))
import Linear.Exts

newtype Transform a b = Transform { getTransform :: M44 Float }
  deriving (Show)

instance Category Transform where
  id = Transform identity
  Transform a . Transform b = Transform (b !*! a)

mkTranslation :: V3 Float -> Transform a b
mkTranslation v = Transform (identity & translation .~ v)

mkRotation :: Quaternion Float -> Transform a b
mkRotation q = Transform (identity !*! mkTransformation q 0)

apply :: Transform a b -> V4 Float -> V4 Float
apply (Transform m) v = m !* v
