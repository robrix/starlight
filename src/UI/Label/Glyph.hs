{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
module UI.Label.Glyph
( shader
, U(..)
, matrix_
, ratio_
, fontScale_
, offset_
, V(..)
, Frag(..)
) where

import Control.Lens (Lens')
import Data.Functor.I
import Data.Generics.Product.Fields
import Foreign.Storable (Storable)
import GHC.Generics (Generic)
import GL.Shader.DSL
import Prelude hiding (break)

shader :: Shader shader => shader U V Frag
shader
  =   vertex (\ U{ matrix, ratio, fontScale, offset } V{ pos } IF{ _coord2, colour } -> main $ do
    _coord2 .= pos^._zw ^* 0.5
    t <- var "t" (v2 0)
    r <- let' "r" (1/float ratio)
    switch gl_InstanceID
      [ (Just 0, do
        colour .= v4 (V4 1 0 0 1)
        t .= v2 (V2 (-1/12.0) (-5/12.0))
        break)
      , (Just 1, do
        colour .= v4 (V4 1 0 0 1)
        t .= v2 (V2  (1/12.0)  (1/12.0))
        break)
      , (Just 2, do
        colour .= v4 (V4 0 1 0 1)
        t .= v2 (V2  (3/12.0) (-1/12.0))
        break)
      , (Just 3, do
        colour .= v4 (V4 0 1 0 1)
        t .= v2 (V2  (5/12.0)  (5/12.0))
        break)
      , (Just 4, do
        colour .= v4 (V4 0 0 1 1)
        t .= v2 (V2  (7/12.0) (-3/12.0))
        break)
      , (Just 5, do
        colour .= v4 (V4 0 0 1 1)
        t .= v2 (V2  (9/12.0)  (3/12.0))
        break)
      ]
    let m =   matrix
          !*! m3 (V3
            (V3 1               0               0)
            (V3 0               1               0)
            (V3 (get t^._x * r) (get t^._y * r) 1))
          !*! m3 (V3
            (V3 fontScale 0         0)
            (V3 0         fontScale 0)
            (V3 0         0         1))
          !*! m3 (V3
            (V3 1              0 0)
            (V3 0              1 0)
            (V3 (float offset) 0 1))
    gl_Position .= ext4 (m !* ext3 (pos^._xy) 1) 0^._xywz)

  >>> fragment (\ _ IF{ _coord2, colour } Frag{ fragColour } -> main $
    iff (_coord2^._x * _coord2^._x - _coord2^._y `gt` 0)
      discard
      (iff gl_FrontFacing
        -- Upper 4 bits: front faces
        -- Lower 4 bits: back faces
        (fragColour .= colour * 16 / 255)
        (fragColour .= colour      / 255)))


data U v = U
  { matrix    :: v (M33 Float)
  , ratio     :: v (I Int)
  , fontScale :: v Float
  , offset    :: v Int
  }
  deriving (Generic)

instance Vars U

matrix_ :: Lens' (U v) (v (M33 Float))
matrix_ = field @"matrix"

ratio_ :: Lens' (U v) (v (I Int))
ratio_ = field @"ratio"

fontScale_ :: Lens' (U v) (v Float)
fontScale_ = field @"fontScale"

offset_ :: Lens' (U v) (v Int)
offset_ = field @"offset"


newtype V v = V { pos :: v (V4 Float) }
  deriving (Generic)

instance Vars V

deriving via Fields V instance Storable (V I)


data IF v = IF
  { _coord2 :: v (V2 Float)
  , colour  :: v (Colour Float)
  }
  deriving (Generic)

instance Vars IF
