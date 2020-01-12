{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
module Linear.Exts
( translated
, translated3
, orient
, face
, easeInOutCubic
, reject
, direction
, angleOf
, angleTo
, isFacing
, toAxisAngle
, cartesian2
, polar2
, Ext(..)
, module Linear.Epsilon
, module Linear.Matrix
, module Linear.Metric
, module Linear.Quaternion
, module Linear.V1
, module Linear.V2
, module Linear.V3
, module Linear.V4
, module Linear.Vector
) where

import Data.Functor.Interval
import Linear.Epsilon
import Linear.Matrix hiding (Trace(..))
import Linear.Metric
import Linear.Quaternion
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

translated3 :: V3 Float -> M44 Float
translated3 (V3 tx ty tz) = V4
  (V4 1 0 0 tx)
  (V4 0 1 0 ty)
  (V4 0 0 1 tz)
  (V4 0 0 0 1)


orient :: (Epsilon a, RealFloat a) => Radians a -> Radians a -> Radians a -> Quaternion a
orient alpha beta gamma
  = axisAngle (unit _z) (getRadians alpha)
  * axisAngle (unit _x) (getRadians beta)
  * axisAngle (unit _z) (getRadians gamma)


-- | Compute a rotation turning to face a desired angle with a given maximum angular thrust.
face
  :: Radians Float    -- ^ Angular thrust. (Speed of rotation.)
  -> Radians Float    -- ^ Desired angle.
  -> Quaternion Float -- ^ Current rotation.
  -> Quaternion Float -- ^ Resulting rotation.
face angular angle rotation = slerp rotation proposed (min 1 (getRadians (angular / delta))) where
  proposed = axisAngle (unit _z) (getRadians angle)
  delta = abs (wrap (Interval (-pi) pi) (snd (toAxisAngle rotation) - angle))


easeInOutCubic :: Float -> Float
easeInOutCubic t
  | t < 0.5   = 4 * t ** 3
  | otherwise = (t - 1) * (2 * t - 2) ** 2 + 1


reject :: (Metric v, Fractional a) => v a -> v a -> v a
reject a b = a ^-^ project a b


-- | The unit vector in the direction of another vector.
direction :: (Metric v, Epsilon a, Floating a) => v a -> v a -> v a
direction a b = normalize (a ^-^ b)


-- | The angle of a vector.
angleOf :: RealFloat a => V2 a -> Radians a
angleOf (V2 x y) = Radians (atan2 y x)

-- | The angle from the first vector to the second.
angleTo :: RealFloat a => V2 a -> V2 a -> Radians a
angleTo v1 v2 = angleOf (v2 - v1)


isFacing :: (Real a, Floating a) => Radians a -> Quaternion a -> Radians a -> Bool
isFacing epsilon rotation target = abs (wrap (Interval (-pi) pi) (snd (toAxisAngle rotation) - target)) < epsilon


-- | Compute the axis/angle of a rotation represented as a unit quaternion.
--
-- NB: Assumes unit magnitude. The axis is undefined for 0-rotations.
toAxisAngle :: (Floating a, Ord a) => Quaternion a -> (V3 a, Radians a)
toAxisAngle (Quaternion qw qv) = (v, Radians phi) where
  v   = sign *^ qv ^/ sqrt (1 - qw ^ (2 :: Int))
  phi = sign * 2 * acos qw
  sign | qv >= 0   =  1
       | otherwise = -1


cartesian2 :: Floating a => Radians a -> a -> V2 a
cartesian2 (Radians phi) r = V2 (r * cos phi) (r * sin phi)

polar2 :: RealFloat a => V2 a -> (Radians a, a)
polar2 v = (angleOf v, norm v) where


-- | Extensions of a vector with an extra dimension.
class Ext v a v' | v v' -> a, v a -> v', v' a -> v where
  ext :: v -> a -> v'

instance Ext (V1 a) a (V2 a) where
  ext (V1 x) = V2 x

instance Ext (V2 a) a (V3 a) where
  ext (V2 x y) = V3 x y

instance Ext a (V2 a) (V3 a) where
  ext x (V2 y z) = V3 x y z

instance Ext (V2 a) (V2 a) (V4 a) where
  ext (V2 x y) (V2 z w) = V4 x y z w

instance Ext a (V3 a) (V4 a) where
  ext x (V3 y z w) = V4 x y z w

instance Ext (V3 a) a (V4 a) where
  ext (V3 x y z) = V4 x y z
