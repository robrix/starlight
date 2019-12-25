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
shader = V vertex $ F fragment Nil where
  vertex U { matrix } I { pos } None =
    gl_Position .= vec4 (vec3 ((matrix !* vec3 pos 1) ^. _xy) 0) 1

  fragment U { colour } None O { fragColour } =
    fragColour .= colour


data U v = U
  { matrix :: v (M33 Float)
  , colour :: v (Colour Float)
  }
  deriving (Generic)

instance Vars U

newtype O v = O { fragColour :: v (Colour Float) }
  deriving (Generic)

instance Vars O
