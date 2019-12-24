{-# LANGUAGE DeriveAnyClass, DeriveGeneric #-}
module UI.Label.Text
( U(..)
) where

import GHC.Generics (Generic)
import GL.Shader.DSL

data U v = U
  { rect    :: v (V4 Float)
  , sampler :: v TextureUnit
  , colour  :: v (Colour Float)
  }
  deriving (Generic, Vars)
