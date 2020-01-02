{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE StandaloneDeriving #-}
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

import Foreign.Storable (Storable)
import GHC.Generics (Generic)
import GL.Object
import GL.Shader.DSL
import Lens.Micro (Lens', lens)
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
matrix_ = lens matrix (\ u matrix -> u { matrix })

radius_ :: Lens' (U v) (v Float)
radius_ = lens radius (\ u radius -> u { radius })

angle_ :: Lens' (U v) (v (Radians Float))
angle_ = lens angle (\ u angle -> u { angle })

sweep_ :: Lens' (U v) (v (Radians Float))
sweep_ = lens sweep (\ u sweep -> u { sweep })

colour_ :: Lens' (U v) (v (Colour Float))
colour_ = lens colour (\ u colour -> u { colour })


newtype V v = V { n :: v Float }
  deriving (Generic)

instance Vars V

deriving instance Bind     (v Float) => Bind     (V v)
deriving instance Storable (v Float) => Storable (V v)

newtype O v = O { fragColour :: v (Colour Float) }
  deriving (Generic)

instance Vars O
