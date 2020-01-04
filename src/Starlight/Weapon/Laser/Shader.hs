{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeApplications #-}
module Starlight.Weapon.Laser.Shader
( shader
, U(..)
, matrix_
, angle_
, colour_
, V(..)
, O(..)
) where

import Control.Lens (Lens')
import Data.Generics.Product.Fields
import Foreign.Storable (Storable)
import GHC.Generics (Generic)
import GL.Object
import GL.Shader.DSL
import Unit.Angle

shader :: Shader U V O
shader = program $ \ u
  ->  vertex (\ V{ r } None ->
    gl_Position .= matrix u !* vec4 r 0 0 1)
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
matrix_ = field @"matrix"

angle_ :: Lens' (U v) (v (Radians Float))
angle_ = field @"angle"

colour_ :: Lens' (U v) (v (Colour Float))
colour_ = field @"colour"


newtype V v = V { r :: v Float }
  deriving (Generic)

instance Vars V

deriving instance Bind     (v Float) => Bind     (V v)
deriving instance Storable (v Float) => Storable (V v)

newtype O v = O { fragColour :: v (Colour Float) }
  deriving (Generic)

instance Vars O
