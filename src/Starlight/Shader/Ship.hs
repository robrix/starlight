{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
module Starlight.Shader.Ship
( shader
, U(..)
, I(..)
, O(..)
) where

import Foreign.Storable (Storable)
import GHC.Generics (Generic)
import GL.Object
import GL.Shader.DSL

shader :: Shader U I O
shader = program $ \ u
  ->  vertex (\ I{ pos } None ->
    gl_Position .= vec4 (matrix u !* vec3 pos 1) 1)

  >>> fragment (\ None O { fragColour } ->
    fragColour .= colour u)


data U v = U
  { matrix :: v (M33 Float)
  , colour :: v (Colour Float)
  }
  deriving (Generic)

instance Vars U

newtype I v = I { pos :: v (V2 Float) }
  deriving (Generic)

instance Vars I

deriving instance Bind     (v (V2 Float)) => Bind     (I v)
deriving instance Storable (v (V2 Float)) => Storable (I v)

newtype O v = O { fragColour :: v (Colour Float) }
  deriving (Generic)

instance Vars O
