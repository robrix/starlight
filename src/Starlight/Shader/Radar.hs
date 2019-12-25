{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
module Starlight.Shader.Radar
( shader
, U(..)
, I(..)
, O(..)
) where

import Foreign.Storable (Storable)
import GHC.Generics (Generic)
import GL.Object
import GL.Shader.DSL
import Unit.Angle (Radians(..))

shader :: Shader U I O
shader = program $ \ u
  ->  vertex (\ I{ n } None -> do
    angle <- let' "angle" (coerce (angle u) + n * coerce (sweep u))
    pos   <- let' "pos"   (vec2 (cos angle) (sin angle) ^* 150)
    gl_Position .= vec4 (vec3 ((matrix u !* vec3 pos 1) ^. _xy) 0) 1)

  >>> fragment (\ None O{ fragColour } -> fragColour .= colour u)


data U v = U
  { matrix :: v (M33 Float)
  , angle  :: v (Radians Float)
  , sweep  :: v (Radians Float)
  , colour :: v (Colour Float)
  }
  deriving (Generic)

instance Vars U

newtype I v = I { n :: v Float }
  deriving (Generic)

instance Vars I

deriving instance Bind     (v Float) => Bind     (I v)
deriving instance Storable (v Float) => Storable (I v)

newtype O v = O { fragColour :: v (Colour Float) }
  deriving (Generic)

instance Vars O
