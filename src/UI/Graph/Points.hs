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
, Frag(..)
) where

import Control.Lens (Lens')
import Data.Generics.Product.Fields
import GHC.Generics (Generic)
import GL.Shader.DSL
import UI.Graph.Vertex

shader :: Shader shader => shader U V Frag
shader
  =   vertex (\ U{ matrix, pointSize } V{ pos } None -> main $ do
    gl_Position .= ext4 (ext3 ((matrix !* ext3 pos 1) ^. _xy) 0) 1
    gl_PointSize .= pointSize)

  >>> fragment (\ U{ colour } None Frag{ fragColour } -> main $ do
    p <- let' "p" (gl_PointCoord - v2 (pure 0.5))
    iff (norm p `gt` 1)
      discard
      (do
        mag <- let' "mag" (norm p * 2)
        fragColour .= ext4 (colour ^. _xyz) (1 - mag ** 3 / 2)))


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
