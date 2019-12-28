{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
module UI.Graph.Lines
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
  ->  vertex (\ V{ pos } None ->
    gl_Position .= ext4 (ext3 ((matrix u !* ext3 pos 1) ^. _xy) 0) 1)

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
