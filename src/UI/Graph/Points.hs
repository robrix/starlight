{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
module UI.Graph.Points
( shader
, U(..)
, V(..)
, O(..)
) where

import GHC.Generics (Generic)
import GL.Shader.DSL
import UI.Graph.Vertex

shader :: Shader U V O
shader = program $ \ u
  ->  vertex (\ V{ pos } None -> do
    gl_Position .= vec4 (vec3 ((matrix u !* vec3 pos 1) ^. _xy) 0) 1
    gl_PointSize .= pointSize u)

  >>> fragment (\ None O{ fragColour } -> do
    p <- let' "p" (gl_PointCoord - vec2 0.5 0.5)
    iff (norm p `gt` 1)
      discard
      (do
        mag <- let' "mag" (norm p * 2)
        fragColour .= vec4 (colour u ^. _xyz) (1 - mag * mag * mag / 2)))


data U v = U
  { matrix    :: v (M33 Float)
  , pointSize :: v Float
  , colour    :: v (Colour Float)
  }
  deriving (Generic)

instance Vars U

newtype O v = O { fragColour :: v (Colour Float) }
  deriving (Generic)

instance Vars O
