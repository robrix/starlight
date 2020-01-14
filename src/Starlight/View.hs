{-# LANGUAGE NamedFieldPuns #-}
module Starlight.View
( View(..)
, scaleToView
, aspectRatio
, deviceSize
, lengthToPixels
  -- * Transforms
, ClipSpace
, ContextSpace
, WindowSpace
, ZoomedSpace
, SystemSpace
, PlayerSpace
, toContext
, toWindow
, toZoomed
, toSystem
, toPlayer
, transformToWindow
, transformToZoomed
, transformToSystem
  -- * Re-exports
, module Geometry.Transform
) where

import Control.Lens ((&), (.~), (^.))
import Geometry.Transform
import Linear.Exts
import Starlight.System
import Unit.Length

data View = View
  { ratio :: Int    -- ^ Ratio of window pixels per context pixel.
  , size  :: V2 Int
  , zoom  :: Float
  , scale :: Float
  , focus :: V2 (Mega Metres Float)
  }

-- | Return a matrix transforming the [[-1,1], [-1,1]] interval to device coordinates.
scaleToView :: (Applicative v, Traversable v, R2 v) => View -> v (v Float)
scaleToView View{ ratio, size } = scaled (pure 1 & _xy .~ 1 / (fromIntegral <$> size) ^* fromIntegral ratio)

aspectRatio :: View -> Float
aspectRatio View{ size } = size'^._x / size'^._y where
  size' = fromIntegral <$> size

deviceSize :: View -> V2 Int
deviceSize View{ ratio, size } = ratio *^ size

lengthToPixels :: View -> Float
lengthToPixels View{ zoom, scale } = 1/zoom * scale


data ClipSpace
data ContextSpace
data WindowSpace
data ZoomedSpace
data PlayerSpace


toContext :: View -> Transform ClipSpace ContextSpace
toContext View{ size } = mkScale (pure 1 & _xy .~ 1 / (fromIntegral <$> size))

toWindow :: View -> Transform ContextSpace WindowSpace
toWindow View{ ratio } = mkScale (pure 1 & _xy .~ fromIntegral ratio)

toZoomed :: View -> Transform WindowSpace ZoomedSpace
toZoomed View{ zoom } = mkScale (pure 1 & _xy .~ pure (1/zoom))

toSystem :: View -> Transform ZoomedSpace SystemSpace
toSystem View{ scale, focus } = mkScale (pure scale) >>> mkTranslation (ext (prj <$> negated focus) 0)

toPlayer :: View -> Transform SystemSpace PlayerSpace
toPlayer View{ focus } = mkTranslation (ext (prj <$> focus) 0)

transformToWindow :: View -> Transform ClipSpace WindowSpace
transformToWindow view = toContext view >>> toWindow view

transformToZoomed :: View -> Transform ClipSpace ZoomedSpace
transformToZoomed view = transformToWindow view >>> toZoomed view

transformToSystem :: View -> Transform ClipSpace SystemSpace
transformToSystem view = transformToZoomed view >>> toSystem view
