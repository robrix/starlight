{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
module Starlight.Shader.Body
( shader
, U(..)
, V(..)
, O(..)
) where

import Foreign.Storable (Storable)
import GHC.Generics (Generic)
import GL.Object
import GL.Shader.DSL

shader :: Shader U V O
shader = program $ \ u
  ->  vertex (\ V{ pos } None ->
    gl_Position .= vec4 (matrix u !* pos) 1)

  >>> fragment (\ None O { fragColour } ->
    fragColour .= colour u)


data U v = U
  { matrix :: v (M33 Float)
  , colour :: v (Colour Float)
  }
  deriving (Generic)

instance Vars U

newtype V v = V { pos :: v (V3 Float) }
  deriving (Generic)

instance Vars V

deriving instance Bind     (v (V3 Float)) => Bind     (V v)
deriving instance Storable (v (V3 Float)) => Storable (V v)

newtype O v = O { fragColour :: v (Colour Float) }
  deriving (Generic)

instance Vars O
