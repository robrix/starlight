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
import GL.Shader.DSL hiding (Field(..))
import Prelude hiding (break)

shader :: Shader U V O
shader = program $ \ u
  ->  vertex (\ V{ pos } IF{ _coord2, colour } -> do
    _coord2 .= pos ^. _zw
    t <- var "t" (vec2 0 0)
    switch gl_InstanceID
      [ (Just 0, do
        colour .= vec4 1 0 0 1
        t .= vec2 (-1/12.0) (-5/12.0)
        break)
      , (Just 1, do
        colour .= vec4 1 0 0 1
        t .= vec2 ( 1/12.0) ( 1/12.0)
        break)
      , (Just 2, do
        colour .= vec4 0 1 0 1
        t .= vec2 ( 3/12.0) (-1/12.0)
        break)
      , (Just 3, do
        colour .= vec4 0 1 0 1
        t .= vec2 ( 5/12.0) ( 5/12.0)
        break)
      , (Just 4, do
        colour .= vec4 0 0 1 1
        t .= vec2 ( 7/12.0) (-3/12.0)
        break)
      , (Just 5, do
        colour .= vec4 0 0 1 1
        t .= vec2 ( 9/12.0) ( 3/12.0)
        break)
      ]
    let trans2 t = mat3 (vec3 1 0 0) (vec3 0 1 0) (vec3 (t ^. _x) (t ^. _y) 1)
        scale2 s = mat3 (vec3 s 0 0) (vec3 0 s 0) (vec3 0 0 1)
        m =   matrix3 u
          !*! trans2 (get t ^* scale u)
          !*! scale2 (fontScale u)
          !*! trans2 (offset u)
    gl_Position .= ext4 (m !* ext3 (pos ^. _xy) 1) 0 ^. _xywz)

  >>> fragment (\ IF{ _coord2, colour } O{ fragColour } -> do
    iff (_coord2 ^. _x * _coord2 ^. _x - _coord2 ^. _y `gt` 0)
      discard
      (iff gl_FrontFacing
        -- Upper 4 bits: front faces
        -- Lower 4 bits: back faces
        (fragColour .= colour * 16 / 255)
        (fragColour .= colour * 1  / 255)))


data U v = U
  { matrix3   :: v (M33 Float)
  , scale     :: v Float
  , fontScale :: v Float
  , offset    :: v (V2 Float)
  }
  deriving (Generic)

instance Vars U

newtype V v = V { pos :: v (V4 Float) }
  deriving (Generic)

instance Vars V

deriving instance Bind     (v (V4 Float)) => Bind     (V v)
deriving instance Storable (v (V4 Float)) => Storable (V v)

data IF v = IF
  { _coord2 :: v (V2 Float)
  , colour  :: v (Colour Float)
  }
  deriving (Generic)

instance Vars IF

newtype O v = O { fragColour :: v (Colour Float) }
  deriving (Generic)

instance Vars O
