{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
module Starlight.Shader.Ship
( shader
, U(..)
, I(..)
, O(..)
) where

import GHC.Generics (Generic)
import GL.Shader.DSL

shader :: Shader U I O
shader = V vertex $ F fragment Nil where
  vertex U { matrix } I { pos } None =
    gl_Position .= vec4 (matrix !* vec3 pos 1) 1

  fragment U { colour } None O { fragColour } =
    fragColour .= colour


data U v = U
  { matrix :: v (M33 Float)
  , colour :: v (Colour Float)
  }
  deriving (Generic, Vars)

newtype I v = I { pos :: v (V2 Float) }
  deriving (Generic, Vars)

newtype O v = O { fragColour :: v (Colour Float) }
  deriving (Generic, Vars)
