{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
module UI.Label.Text
( shader
, U(..)
, I(..)
, O(..)
) where

import Foreign.Storable (Storable)
import GHC.Generics (Generic)
import GL.Object
import GL.Shader.DSL

shader :: Shader U I O
shader = V vertex $ F fragment Nil where
  vertex U { rect } I { pos } IF { _coord2 } = do
    v <- let' "v" $ lerp2 (pos * 0.5 + 0.5) (rect ^. _xy) (rect ^. _zw)
    _coord2 .= v
    gl_Position .= vec4 (vec3 (v * 2 - 1) 0) 1

  fragment U { sampler, colour } IF { _coord2 } O { fragColour } = do
    -- Get samples for -2/3 and -1/3
    valueL <- let' "valueL" $ texture sampler (vec2 (_coord2 ^. _x + dFdx (_coord2 ^. _x)) (_coord2 ^. _y)) ^. _yz * 255
    lowerL <- let' "lowerL" $ mod' valueL 16
    upperL <- let' "upperL" $ (valueL - lowerL) / 16
    alphaL <- let' "alphaL" $ min' (abs (upperL - lowerL)) 2

    -- Get samples for 0, +1/3, and +2/3
    valueR <- let' "valueR" $ texture sampler _coord2 ^. _xyz * 255
    lowerR <- let' "lowerR" $ mod' valueR 16
    upperR <- let' "upperR" $ (valueR - lowerR) / 16
    alphaR <- let' "alphaR" $ min' (abs (upperR - lowerR)) 2

    -- Average the energy over the pixels on either side
    rgba <- let' "rgba" $ vec4 (vec3 (vec2
      ((alphaR ^. _x + alphaR ^. _y + alphaR ^. _z) / 6)
      ((alphaL ^. _y + alphaR ^. _x + alphaR ^. _y) / 6))
      ((alphaL ^. _x + alphaL ^. _y + alphaR ^. _x) / 6))
      0

    iff (colour ^. _x `eq` 0)
      (fragColour .= 1 - rgba)
      (fragColour .= colour * rgba)



data U v = U
  { rect    :: v (V4 Float)
  , sampler :: v TextureUnit
  , colour  :: v (Colour Float)
  }
  deriving (Generic)

instance Vars U

newtype I v = I { pos :: v (V2 Float) }
  deriving (Generic)

instance Vars I

deriving instance Bind     (v (V2 Float)) => Bind     (I v)
deriving instance Storable (v (V2 Float)) => Storable (I v)

newtype IF v = IF { _coord2 :: v (V2 Float) }
  deriving (Generic)

instance Vars IF

newtype O v = O { fragColour :: v (Colour Float) }
  deriving (Generic)

instance Vars O
