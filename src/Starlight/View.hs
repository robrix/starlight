{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module Starlight.View
( View(..)
, contextSize
, lengthToWindowPixels
, zoomForSpeed
  -- * Transforms
, ClipUnits(..)
, Zoomed(..)
, transformToWindow
, transformToZoomed
, transformToSystem
  -- * Viewport
, clipTo
  -- * Re-exports
, module Geometry.Transform
) where

import Control.Effect.Lift
import Control.Lens ((&), (.~))
import Data.Coerce
import Data.Functor.I
import Data.Functor.Interval
import Data.Functor.K
import Foreign.Storable
import Geometry.Transform
import GL.Type as GL
import GL.Uniform
import GL.Viewport
import Linear.Conjugate
import Linear.Exts
import UI.Context as Context
import UI.Window as Window
import Unit.Algebra
import Unit.Length
import Unit.Time

data View = View
  { ratio :: Int    -- ^ Ratio of window pixels per context pixel.
  , size  :: V2 (Window.Pixels Int)
  , zoom  :: I Double
  , scale :: (Window.Pixels :/: Mega Metres) Double
  , focus :: V2 (Mega Metres Double)
  }

contextSize :: View -> V2 (Context.Pixels Int)
contextSize View{ ratio, size } = fmap Context.Pixels (ratio *^ fmap Window.getPixels size)

lengthToWindowPixels :: View -> (Window.Pixels :/: Mega Metres) Double
lengthToWindowPixels View{ zoom, scale } = scale .*. zoom

-- | Compute the zoom factor for the given velocity.
--
-- Higher values correlate to more of the scene being visible.
zoomForSpeed :: V2 (Window.Pixels Int) -> (Mega Metres :/: Seconds) Double -> I Double
zoomForSpeed size x
  | distance < min' bounds = min' zoom
  | distance > max' bounds = max' zoom
  | otherwise              = fromUnit zoom (coerce easeInOutCubic (toUnit bounds distance))
  where
  hypotenuse = norm (fmap fromIntegral <$> size)
  distance = I (x .*. Seconds 1 ./. hypotenuse) -- how much of the screen will be traversed in a second
  zoom = interval 1 (1/5)
  bounds = interval 1 (20 :: Mega Metres Double) ^/. hypotenuse


newtype ClipUnits a = ClipUnits { getClipUnits :: a }
  deriving (Column, Conjugate, Enum, Epsilon, Eq, Foldable, Floating, Fractional, Functor, Integral, Num, Ord, Real, RealFloat, RealFrac, Row, Show, Storable, Traversable, GL.Type, Uniform)
  deriving (Additive, Applicative, Metric, Monad) via I

instance Unit ClipUnits where
  type Dim ClipUnits = Length
  suffix = K ("clip"++)

newtype Zoomed a = Zoomed { getZoomed :: a }
  deriving (Column, Conjugate, Enum, Epsilon, Eq, Foldable, Floating, Fractional, Functor, Integral, Num, Ord, Real, RealFloat, RealFrac, Row, Show, Storable, Traversable, GL.Type, Uniform)
  deriving (Additive, Applicative, Metric, Monad) via I

instance Unit Zoomed where
  type Dim Zoomed = Length
  suffix = K ("zoomed"++)


transformToWindow :: View -> Transform Double ClipUnits Window.Pixels
transformToWindow View{ size, ratio }
  = mkScale (pure 1 & _xy .~ ClipUnits (fromIntegral ratio) ./^ (fmap fromIntegral <$> size))

transformToZoomed :: View -> Transform Double ClipUnits Window.Pixels
transformToZoomed view@View{ zoom }
  =   transformToWindow view
  >>> mkScale (pure 1 & _xy .~ pure zoom)

transformToSystem :: View -> Transform Double ClipUnits (Mega Metres)
transformToSystem view@View{ scale, focus }
  =   transformToZoomed view
  >>> mkScale (pure scale)
  >>> mkTranslation (ext (negated focus) 0)


clipTo :: Has (Lift IO) sig m => View -> m ()
clipTo view = do
  viewport $ Interval 0 dsize
  scissor  $ Interval 0 dsize where
  dsize = contextSize view
