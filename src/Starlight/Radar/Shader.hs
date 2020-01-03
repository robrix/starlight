{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
module Starlight.Radar.Shader
( shader
, U(..)
, matrix_
, radius_
, angle_
, sweep_
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
import Unit.Angle (Radians(..))

shader :: Shader U V O
shader = program $ \ u
  ->  vertex (\ V{ n } None -> do
    angle <- let' "angle" (coerce (angle u) + n * coerce (sweep u))
    pos   <- let' "pos"   (vec2 (cos angle) (sin angle) ^* radius u)
    gl_PointSize .= 3
    gl_Position .= ext4 (ext3 ((matrix u !* ext3 pos 1) ^. _xy) 0) 1)

  >>> fragment (\ None O{ fragColour } -> fragColour .= colour u)


data U v = U
  { matrix :: v (M33 Float)
  , radius :: v Float
  , angle  :: v (Radians Float)
  , sweep  :: v (Radians Float)
  , colour :: v (Colour Float)
  }
  deriving (Generic)

instance Vars U

matrix_ :: Lens' (U v) (v (M33 Float))
matrix_ = field @"matrix"

radius_ :: Lens' (U v) (v Float)
radius_ = field @"radius"

angle_ :: Lens' (U v) (v (Radians Float))
angle_ = field @"angle"

sweep_ :: Lens' (U v) (v (Radians Float))
sweep_ = field @"sweep"

colour_ :: Lens' (U v) (v (Colour Float))
colour_ = field @"colour"


newtype V v = V { n :: v Float }
  deriving (Generic)

instance Vars V

deriving instance Bind     (v Float) => Bind     (V v)
deriving instance Storable (v Float) => Storable (V v)

newtype O v = O { fragColour :: v (Colour Float) }
  deriving (Generic)

instance Vars O
