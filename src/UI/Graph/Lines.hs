{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
module UI.Graph.Lines
( shader
, U(..)
, matrix_
, colour_
, V(..)
, O(..)
) where

import Control.Lens (Lens', lens)
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

matrix_ :: Lens' (U v) (v (M33 Float))
matrix_ = lens matrix (\ u matrix -> u { matrix })

colour_ :: Lens' (U v) (v (Colour Float))
colour_ = lens colour (\ u colour -> u { colour })


newtype O v = O { fragColour :: v (Colour Float) }
  deriving (Generic)

instance Vars O
