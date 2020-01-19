{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
module Starlight.View
( View(..)
, aspectRatio
, deviceSize
, lengthToWindowPixels
  -- * Transforms
, ClipUnits(..)
, Zoomed(..)
, toContext
, toWindow
, toZoomed
, toSystem
, transformToWindow
, transformToZoomed
, transformToSystem
  -- * Viewport
, clipTo
  -- * Re-exports
, module Geometry.Transform
) where

import Control.Effect.Lift
import Control.Lens ((&), (.~), (^.))
import Data.Functor.Interval
import Geometry.Transform
import GL.Viewport
import Linear.Exts
import UI.Context as Context
import UI.Window as Window
import Unit.Length

data View = View
  { ratio :: Int    -- ^ Ratio of window pixels per context pixel.
  , size  :: V2 (Window.Pixels Int)
  , zoom  :: Double
  , scale :: Double
  , focus :: V2 (Mega Metres Double)
  }

aspectRatio :: View -> Float
aspectRatio View{ size } = size'^._x / size'^._y where
  size' = fromIntegral <$> size

deviceSize :: View -> V2 (Context.Pixels Int)
deviceSize View{ ratio, size } = fmap Context.Pixels (ratio *^ fmap Window.getPixels size)

lengthToWindowPixels :: View -> Double
lengthToWindowPixels View{ zoom, scale } = 1/zoom * scale


newtype ClipUnits a = ClipUnits { getClipUnits :: a }
newtype Zoomed a = Zoomed { getZoomed :: a }


toContext :: View -> Transform Double ClipUnits Context.Pixels
toContext View{ size } = mkScale (pure 1 & _xy .~ 1 / (fromIntegral <$> size))

toWindow :: View -> Transform Double Context.Pixels Window.Pixels
toWindow View{ ratio } = mkScale (pure 1 & _xy .~ fromIntegral ratio)

toZoomed :: View -> Transform Double Window.Pixels Zoomed
toZoomed View{ zoom } = mkScale (pure 1 & _xy .~ pure (1/zoom))

toSystem :: View -> Transform Double Zoomed (Mega Metres)
toSystem View{ scale, focus } = mkScale (pure scale) >>> mkTranslation (ext (negated focus) 0)

transformToWindow :: View -> Transform Double ClipUnits Window.Pixels
transformToWindow view = toContext view >>> toWindow view

transformToZoomed :: View -> Transform Double ClipUnits Zoomed
transformToZoomed view = transformToWindow view >>> toZoomed view

transformToSystem :: View -> Transform Double ClipUnits (Mega Metres)
transformToSystem view = transformToZoomed view >>> toSystem view


clipTo :: Has (Lift IO) sig m => View -> m ()
clipTo view = do
  viewport $ Interval 0 dsize
  scissor  $ Interval 0 dsize where
  dsize = deviceSize view
