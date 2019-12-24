{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
module UI.Label.Glyph
( shader
, U(..)
) where

import GHC.Generics (Generic)
import GL.Shader.DSL

shader :: Shader U I O
shader = V vertex $ F fragment Nil where
  vertex U { matrix3 } I { pos } IF { _coord2 } = do
    _coord2 .= pos ^. _zw
    gl_Position .= vec4 (matrix3 !* vec3 (pos ^. _xy) 1) 0 ^. _xywz

  fragment U { colour } IF { _coord2 } O { fragColour } = do
    iff (_coord2 ^. _x * _coord2 ^. _x - _coord2 ^. _y `gt` 0)
      discard
      (iff gl_FrontFacing
        -- Upper 4 bits: front faces
        -- Lower 4 bits: back faces
        (fragColour .= colour * 16 / 255)
        (fragColour .= colour * 1 / 255))


data U v = U
  { matrix3 :: v (M33 Float)
  , colour  :: v (Colour Float)
  }
  deriving (Generic, Vars)

newtype I v = I { pos :: v (V4 Float) }
  deriving (Generic, Vars)

newtype IF v = IF { _coord2 :: v (V2 Float) }
  deriving (Generic, Vars)

newtype O v = O { fragColour :: v (Colour Float) }
  deriving (Generic, Vars)
