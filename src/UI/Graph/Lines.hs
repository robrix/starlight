{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}
module UI.Graph.Lines
( shader
, U(..)
, matrix_
, colour_
, V(..)
, Frag(..)
) where

import Control.Lens (Lens')
import Data.Generics.Product.Fields
import GHC.Generics (Generic)
import GL.Shader.DSL
import UI.Graph.Vertex

shader :: Shader U V Frag
shader = program $ \ u
  ->  vertex (\ V{ pos } None -> main $
    gl_Position .= ext4 (ext3 ((matrix u !* ext3 pos 1) ^. _xy) 0) 1)

  >>> fragment (\ None Frag{ fragColour } -> main $
    fragColour .= colour u)


data U v = U
  { matrix :: v (M33 Float)
  , colour :: v (Colour Float)
  }
  deriving (Generic)

instance Vars U

matrix_ :: Lens' (U v) (v (M33 Float))
matrix_ = field @"matrix"

colour_ :: Lens' (U v) (v (Colour Float))
colour_ = field @"colour"
