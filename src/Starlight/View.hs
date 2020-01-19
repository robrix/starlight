{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies #-}
module Starlight.View
( View(..)
, aspectRatio
, contextSize
, lengthToWindowPixels
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
import Control.Lens ((&), (.~), (^.))
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

contextSize :: View -> V2 (Context.Pixels Int)
contextSize View{ ratio, size } = fmap Context.Pixels (ratio *^ fmap Window.getPixels size)

lengthToWindowPixels :: View -> Double
lengthToWindowPixels View{ zoom, scale } = 1/zoom * scale


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

transformToZoomed :: View -> Transform Double ClipUnits Zoomed
transformToZoomed view@View{ zoom }
  =   transformToWindow view
  >>> mkScale (pure 1 & _xy .~ pure (Window.Pixels 1 ./. Zoomed zoom))

transformToSystem :: View -> Transform Double ClipUnits (Mega Metres)
transformToSystem view@View{ scale, focus }
  =   transformToZoomed view
  >>> mkScale (pure (pure scale))
  >>> mkTranslation (ext (negated focus) 0)


clipTo :: Has (Lift IO) sig m => View -> m ()
clipTo view = do
  viewport $ Interval 0 dsize
  scissor  $ Interval 0 dsize where
  dsize = contextSize view
