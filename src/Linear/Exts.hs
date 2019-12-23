{-# LANGUAGE FlexibleInstances, FunctionalDependencies #-}
module Linear.Exts
( translated
, rotated
, scaled
, reject
, angleOf
, angleTo
, cartesian2
, polar2
, Ext(..)
) where

import Linear.Matrix
import Linear.Metric
import Linear.V1
import Linear.V2
import Linear.V3
import Linear.V4
import Linear.Vector
import Unit.Angle

translated :: V2 Float -> M33 Float
translated (V2 tx ty) = V3
  (V3 1 0 tx)
  (V3 0 1 ty)
  (V3 0 0 1)

rotated :: Radians Float -> M33 Float
rotated (Radians theta) = V3
  (V3 cosT (-sinT) 0)
  (V3 sinT cosT    0)
  (V3 0    0       1) where
  cosT = cos theta
  sinT = sin theta


reject :: (Metric v, Fractional a) => v a -> v a -> v a
reject a b = a ^-^ project a b


-- | The angle of a vector.
angleOf :: RealFloat a => V2 a -> Radians a
angleOf (V2 x y) = Radians (atan2 y x)

-- | The angle from the first vector to the second.
angleTo :: RealFloat a => V2 a -> V2 a -> Radians a
angleTo v1 v2 = angleOf (v2 - v1)


cartesian2 :: Floating a => Radians a -> a -> V2 a
cartesian2 (Radians phi) r = V2 (r * cos phi) (r * sin phi)

polar2 :: RealFloat a => V2 a -> (Radians a, a)
polar2 v = (angleOf v, norm v) where


-- | Extensions of a vector with an extra dimension.
class Ext v a v' | v -> v' a, v' -> v a where
  ext :: v -> a -> v'

instance Ext (V1 a) a (V2 a) where
  ext (V1 x) = V2 x

instance Ext (V2 a) a (V3 a) where
  ext (V2 x y) = V3 x y

instance Ext (V3 a) a (V4 a) where
  ext (V3 x y z) = V4 x y z
