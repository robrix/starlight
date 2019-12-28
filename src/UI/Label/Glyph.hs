{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
module UI.Label.Glyph
( shader
, U(..)
, V(..)
, O(..)
) where

import Foreign.Storable (Storable)
import GHC.Generics (Generic)
import GL.Object
import GL.Shader.DSL

shader :: Shader U V O
shader = program $ \ u
  ->  vertex (\ V{ pos } IF{ _coord2 } -> do
    _coord2 .= pos ^. _zw
    gl_Position .= ext4 (matrix3 u !* ext3 (pos ^. _xy) 1) 0 ^. _xywz)

  >>> fragment (\ IF{ _coord2 } O{ fragColour } -> do
    iff (_coord2 ^. _x * _coord2 ^. _x - _coord2 ^. _y `gt` 0)
      discard
      (iff gl_FrontFacing
        -- Upper 4 bits: front faces
        -- Lower 4 bits: back faces
        (fragColour .= colour u * 16 / 255)
        (fragColour .= colour u * 1 / 255)))


data U v = U
  { matrix3 :: v (M33 Float)
  , colour  :: v (Colour Float)
  }
  deriving (Generic)

instance Vars U

newtype V v = V { pos :: v (V4 Float) }
  deriving (Generic)

instance Vars V

deriving instance Bind     (v (V4 Float)) => Bind     (V v)
deriving instance Storable (v (V4 Float)) => Storable (V v)

newtype IF v = IF { _coord2 :: v (V2 Float) }
  deriving (Generic)

instance Vars IF

newtype O v = O { fragColour :: v (Colour Float) }
  deriving (Generic)

instance Vars O
