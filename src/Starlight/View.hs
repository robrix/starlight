{-# LANGUAGE NamedFieldPuns #-}
module Starlight.View
( View(..)
, scaleToView
, scaleToViewZoomed
, aspectRatio
, deviceSize
) where

import Control.Lens ((.~), (^.))
import Data.Function ((&))
import Linear.Matrix
import Linear.V2
import Linear.Vector
import Unit.Length

data View = View
  { ratio :: Int    -- ^ Ratio of window pixels per context pixel.
  , size  :: V2 Int
  , zoom  :: Float
  , scale :: Float
  , focus :: V2 (Kilo Metres Float)
  }

-- | Return a matrix transforming the [[-1,1], [-1,1]] interval to device coordinates.
scaleToView :: (Applicative v, Traversable v, R2 v) => View -> v (v Float)
scaleToView View{ ratio, size } = scaled (pure 1 & _xy .~ 1 / (fromIntegral <$> size) ^* fromIntegral ratio)

-- | Return a matrix transforming the [[-1,1], [-1,1]] interval to zoomed device coordinates.
scaleToViewZoomed :: (Additive v, Applicative v, Traversable v, R2 v) => View -> v (v Float)
scaleToViewZoomed vs@View{ zoom } = scaleToView vs !*! scaled (pure 1 & _xy .~ pure (1 / zoom))

aspectRatio :: View -> Float
aspectRatio View{ size } = size' ^. _x / size' ^. _y where
  size' = fromIntegral <$> size

deviceSize :: View -> V2 Int
deviceSize View{ ratio, size } = ratio *^ size
