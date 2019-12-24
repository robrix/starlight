{-# LANGUAGE DeriveAnyClass, DeriveGeneric #-}
module UI.Label.Glyph
( U(..)
) where

import GHC.Generics (Generic)
import GL.Shader.DSL

data U v = U
  { matrix3 :: v (M33 Float)
  , colour  :: v (Colour Float)
  }
  deriving (Generic, Vars)
