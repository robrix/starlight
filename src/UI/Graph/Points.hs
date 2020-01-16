{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}
module UI.Graph.Points
( shader
, U(..)
, matrix_
, pointSize_
, colour_
, V(..)
, O(..)
) where

import Control.Lens (Lens')
import Data.Generics.Product.Fields
import GHC.Generics (Generic)
import GL.Shader.DSL
import UI.Graph.Vertex

shader :: Shader U V O
shader = program $ \ u
  ->  vertex (\ V{ pos } None -> main $ do
    gl_Position .= ext4 (ext3 ((matrix u !* ext3 pos 1) ^. _xy) 0) 1
    gl_PointSize .= pointSize u)

  >>> fragment (\ None O{ fragColour } -> main $ do
    p <- let' "p" (gl_PointCoord - vec2 [0.5])
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
matrix_ = field @"matrix"

pointSize_ :: Lens' (U v) (v Float)
pointSize_ = field @"pointSize"

colour_ :: Lens' (U v) (v (Colour Float))
colour_ = field @"colour"


newtype O v = O { fragColour :: v (Colour Float) }
  deriving (Generic)

instance Vars O
