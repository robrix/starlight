{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RankNTypes #-}
module Linear.Exts
( translated
, orient
, face
, easeInOutCubic
, reject
, direction
, angleOf
, angleTo
, facingRel
, toAxisAngle
, cartesian2
, Ext(..)
, extended
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

import Control.Lens (Iso, iso, (^.))
import Data.Functor.I
import Data.Functor.Interval
import Linear.Epsilon
import Linear.Matrix hiding (Trace(..))
import Linear.Metric
import Linear.Quaternion
import Linear.V1
import Linear.V2 hiding (angle)
import Linear.V3
import Linear.V4
import Linear.Vector
import Unit
import Unit.Algebra

translated :: Num a => V2 a -> M33 a
translated (V2 tx ty) = V3
  (V3 1 0 tx)
  (V3 0 1 ty)
  (V3 0 0 1)


orient :: (Epsilon a, RealFloat a) => a -> a -> a -> Quaternion a
orient alpha beta gamma
  = axisAngle (unit _z) alpha
  * axisAngle (unit _x) beta
  * axisAngle (unit _z) gamma


-- | Compute a rotation turning to face a desired angle with a given maximum angular thrust.
face
  :: (Epsilon a, RealFloat a)
  => I a              -- ^ Angular thrust. (Speed of rotation.)
  -> I a              -- ^ Desired angle.
  -> Quaternion (I a) -- ^ Current rotation.
  -> Quaternion (I a) -- ^ Resulting rotation.
face angular angle rotation
  | nearZero delta = proposed
  | otherwise      = slerp rotation proposed (min 1 (angular / delta)) where
  proposed = axisAngle (unit _z) angle
  delta = facingRel rotation angle


easeInOutCubic :: Double -> Double
easeInOutCubic t
  | t < 0.5   = 4 * t ** 3
  | otherwise = (t - 1) * (2 * t - 2) ** 2 + 1


reject :: (Metric v, Fractional a) => v a -> v a -> v a
reject a b = a ^-^ project a b


-- | The unit vector in the direction of another vector.
direction :: (Metric v, Epsilon a, Floating a, Unit du u) => v (u a) -> v (u a) -> v (I a)
direction a b = normalizeU (a ^-^ b)


-- | The angle of a vector.
angleOf :: (RealFloat a, Unit du u) => V2 (u a) -> I a
angleOf v = I (atan2 y x) where
  V2 x y = prj <$> v

-- | The angle from the first vector to the second.
angleTo :: (RealFloat a, Unit du u) => V2 (u a) -> V2 (u a) -> I a
angleTo v1 v2 = angleOf (v2 - v1)


-- | Compute the angle between a rotation and a proposed angle.
--
-- The result lies in the interval [-pi, pi].
facingRel :: (Real a, Floating a) => Quaternion (I a) -> I a -> I a
facingRel rotation target = abs (wrap (-pi...pi) (snd (toAxisAngle rotation) - target))


-- | Compute the axis/angle of a rotation represented as a unit quaternion.
--
-- NB: Assumes unit magnitude. The axis is undefined for 0-rotations.
toAxisAngle :: (Floating a, Ord a) => Quaternion (I a) -> (V3 (I a), I a)
toAxisAngle (Quaternion qw qv) = (v, phi) where
  v   = sign *^ qv ^/ sqrt (1 - qw ^ (2 :: Int))
  phi = sign * 2 * acos qw
  sign | qv >= 0   =  1
       | otherwise = -1


cartesian2 :: (Floating a, Unit du u) => I a -> u a -> V2 (u a)
cartesian2 phi r = V2 (r .*. cos phi) (r .*. sin phi)


-- | Extensions of a vector with an extra dimension.
class Ext v v' | v -> v', v' -> v where
  ext :: v a -> a -> v' a
  unext :: v' a -> v a

instance Ext V1 V2 where
  ext (V1 x) = V2 x
  unext = V1 . (^._x)

instance Ext V2 V3 where
  ext (V2 x y) = V3 x y
  unext = (^._xy)

instance Ext V3 V4 where
  ext (V3 x y z) = V4 x y z
  unext = (^._xyz)

-- | Subject to the invariant that w=1.
extended :: Ext v v' => a -> Iso (v a) (v b) (v' a) (v' b)
extended a = iso (`ext` a) unext
