{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
module Starlight.Weapon.Laser.Shader
( shader
, U(..)
, matrix_
, angle_
, colour_
, V(..)
, O(..)
) where

import Foreign.Storable (Storable)
import GHC.Generics (Generic)
import GL.Object
import GL.Shader.DSL
import Lens.Micro (Lens', lens)
import Unit.Angle

shader :: Shader U V O
shader = program $ \ u
  ->  vertex (\ V{ r } None -> do
    rot <- let' "rot1" $ mat2 (vec2 (cos (coerce (angle u))) (sin (coerce (angle u)))) (vec2 (-(sin (coerce (angle u)))) (cos (coerce (angle u))))
    gl_Position .= ext4 (ext3 (rot !* vec2 r 0) 0) 1)
  >>> fragment (\ None O{ fragColour } -> do
    fragColour .= colour u)


data U v = U
  { matrix :: v (M44 Float)
  , angle  :: v (Radians Float)
  , colour :: v (Colour Float)
  }
  deriving (Generic)

instance Vars U

matrix_ :: Lens' (U v) (v (M44 Float))
matrix_ = lens matrix (\ u matrix -> u { matrix })

angle_ :: Lens' (U v) (v (Radians Float))
angle_ = lens angle (\ u angle -> u { angle })

colour_ :: Lens' (U v) (v (Colour Float))
colour_ = lens colour (\ u colour -> u { colour })


newtype V v = V { r :: v Float }
  deriving (Generic)

instance Vars V

deriving instance Bind     (v Float) => Bind     (V v)
deriving instance Storable (v Float) => Storable (V v)

newtype O v = O { fragColour :: v (Colour Float) }
  deriving (Generic)

instance Vars O
