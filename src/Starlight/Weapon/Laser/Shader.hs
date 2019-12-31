{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
module Starlight.Weapon.Laser.Shader
( shader
, U(..)
, V(..)
, O(..)
) where

import Foreign.Storable (Storable)
import GHC.Generics (Generic)
import GL.Object
import GL.Shader.DSL
import Unit.Angle

shader :: Shader U V O
shader = program $ \ u
  ->  vertex (\ V{ r } None -> do
    rot <- let' "rot1" $ mat2 (vec2 (cos r) (sin r)) (vec2 (-(sin r)) (cos r))
    gl_Position .= ext4 (ext3 (rot !* vec2 (coerce (angle u)) r) 0) 1)
  >>> fragment (\ None O{ fragColour } -> do
    fragColour .= colour u)


data U v = U
  { matrix :: v (M44 Float)
  , angle  :: v (Radians Float)
  , colour :: v (Colour Float)
  }
  deriving (Generic)

instance Vars U

newtype V v = V { r :: v Float }
  deriving (Generic)

instance Vars V

deriving instance Bind     (v Float) => Bind     (V v)
deriving instance Storable (v Float) => Storable (V v)

newtype O v = O { fragColour :: v (Colour Float) }
  deriving (Generic)

instance Vars O
