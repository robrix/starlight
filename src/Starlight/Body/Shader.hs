{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
module Starlight.Body.Shader
( shader
, U(..)
, V(..)
, O(..)
) where

import Foreign.Storable (Storable)
import GHC.Generics (Generic)
import GL.Object
import GL.Shader.DSL
import Prelude hiding (break)

shader :: Shader U V O
shader = program $ \ u
  ->  vertex (\ V{ pos } None -> do
    let cos90 = 6.123233995736766e-17
    m <- var "m" (matrix u)
    switch gl_InstanceID
      [ (Just 1, m *= mat4 (vec4 1 0 0 0) (vec4 0 cos90 (-1) 0) (vec4 0 1 cos90 0) (vec4 0 0 0 1) >> break)
      , (Just 2, m *= mat4 (vec4 cos90 0 1 0) (vec4 0 1 0 0) (vec4 (-1) 0 cos90 0) (vec4 0 0 0 1) >> break)
      ]
    gl_Position .= get m !* pos)

  >>> fragment (\ None O { fragColour } ->
    fragColour .= colour u)


data U v = U
  { matrix :: v (M44 Float)
  , colour :: v (Colour Float)
  }
  deriving (Generic)

instance Vars U

newtype V v = V { pos :: v (V4 Float) }
  deriving (Generic)

instance Vars V

deriving instance Bind     (v (V4 Float)) => Bind     (V v)
deriving instance Storable (v (V4 Float)) => Storable (V v)

newtype O v = O { fragColour :: v (Colour Float) }
  deriving (Generic)

instance Vars O
