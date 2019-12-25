{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
module UI.Graph.Lines
( shader
, U(..)
, I(..)
, O(..)
) where

import GHC.Generics (Generic)
import GL.Shader.DSL
import UI.Graph.Vertex

shader :: Shader U I O
shader = program $ \ u
  ->  vertex (\ I{ pos } None ->
    gl_Position .= vec4 (vec3 ((matrix u !* vec3 pos 1) ^. _xy) 0) 1)

  >>> fragment (\ None O{ fragColour } ->
    fragColour .= colour u)


data U v = U
  { matrix :: v (M33 Float)
  , colour :: v (Colour Float)
  }
  deriving (Generic)

instance Vars U

newtype O v = O { fragColour :: v (Colour Float) }
  deriving (Generic)

instance Vars O
