{-# LANGUAGE KindSignatures #-}
module Geometry.Transform
( Transform(..)
, mkTranslation
, mkScale
, mkRotation
, apply
, (>>>)
) where

import Control.Category
import Control.Lens ((&), (.~))
import Linear.Exts

newtype Transform (a :: * -> *) (b :: * -> *) = Transform { getTransform :: M44 Float }
  deriving (Show)

instance Category Transform where
  id = Transform identity
  Transform a . Transform b = Transform (b !*! a)

mkTranslation :: V3 Float -> Transform a b
mkTranslation v = Transform (identity & translation .~ v)

-- FIXME: scaling should introduce a change of units
mkScale :: V3 Float -> Transform a b
mkScale v = Transform (scaled (ext v 1))

mkRotation :: Quaternion Float -> Transform a b
mkRotation q = Transform (identity !*! mkTransformation q 0)

apply :: Transform a b -> V4 Float -> V4 Float
apply (Transform m) v = m !* v
