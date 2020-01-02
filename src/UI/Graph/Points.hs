{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
module UI.Graph.Points
( shader
, U(..)
, matrix_
, pointSize_
, colour_
, V(..)
, O(..)
) where

import GHC.Generics (Generic)
import GL.Shader.DSL
import Lens.Micro (Lens', lens)
import UI.Graph.Vertex

shader :: Shader U V O
shader = program $ \ u
  ->  vertex (\ V{ pos } None -> do
    gl_Position .= ext4 (ext3 ((matrix u !* ext3 pos 1) ^. _xy) 0) 1
    gl_PointSize .= pointSize u)

  >>> fragment (\ None O{ fragColour } -> do
    p <- let' "p" (gl_PointCoord - vec2 0.5 0.5)
    iff (norm p `gt` 1)
      discard
      (do
        mag <- let' "mag" (norm p * 2)
        fragColour .= ext4 (colour u ^. _xyz) (1 - mag * mag * mag / 2)))


data U v = U
  { matrix    :: v (M33 Float)
  , pointSize :: v Float
  , colour    :: v (Colour Float)
  }
  deriving (Generic)

instance Vars U

matrix_ :: Lens' (U v) (v (M33 Float))
matrix_ = lens matrix (\ u matrix -> u { matrix })

pointSize_ :: Lens' (U v) (v Float)
pointSize_ = lens pointSize (\ u pointSize -> u { pointSize })

colour_ :: Lens' (U v) (v (Colour Float))
colour_ = lens colour (\ u colour -> u { colour })


newtype O v = O { fragColour :: v (Colour Float) }
  deriving (Generic)

instance Vars O
