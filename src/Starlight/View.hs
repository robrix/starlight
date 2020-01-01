{-# LANGUAGE NamedFieldPuns #-}
module Starlight.View
( ViewScale(..)
, scaleToView
, scaleToViewZoomed
, aspectRatio
) where

import Data.Function ((&))
import Lens.Micro ((.~), (^.))
import Linear.Matrix
import Linear.V2
import Linear.Vector

data ViewScale = ViewScale
  { scale :: Int
  , size  :: V2 Int
  , zoom  :: Float
  }

-- | Return a matrix transforming the [[-1,1], [-1,1]] interval to device coordinates.
scaleToView :: (Applicative v, Traversable v, R2 v) => ViewScale -> v (v Float)
scaleToView ViewScale{ scale, size } = scaled (pure 1 & _xy .~ (1 / (fromIntegral <$> size) ^* fromIntegral scale))

-- | Return a matrix transforming the [[-1,1], [-1,1]] interval to zoomed device coordinates.
scaleToViewZoomed :: (Additive v, Applicative v, Traversable v, R2 v) => ViewScale -> v (v Float)
scaleToViewZoomed vs@ViewScale{ zoom } = scaleToView vs !*! scaled (pure 1 & _xy .~ pure (1 / zoom))

aspectRatio :: ViewScale -> Float
aspectRatio ViewScale{ size } = size' ^. _x / size' ^. _y where
  size' = fromIntegral <$> size
