{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
module Starlight.View
( View(..)
, aspectRatio
, deviceSize
, lengthToWindowPixels
  -- * Transforms
, ClipSpace
, ContextPixels(..)
, WindowPixels(..)
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
  -- * Viewport
, clipTo
  -- * Re-exports
, module Geometry.Transform
) where

import Control.Effect.Lift
import Control.Lens ((&), (.~), (^.))
import Data.Functor.Identity
import Data.Functor.Interval
import Foreign.Storable
import Geometry.Transform
import GL.Type as GL
import GL.Uniform
import GL.Viewport
import Linear.Conjugate
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

aspectRatio :: View -> Float
aspectRatio View{ size } = size'^._x / size'^._y where
  size' = fromIntegral <$> size

deviceSize :: View -> V2 Int
deviceSize View{ ratio, size } = ratio *^ size

lengthToWindowPixels :: View -> Float
lengthToWindowPixels View{ zoom, scale } = 1/zoom * scale


data ClipSpace a

newtype ContextPixels a = ContextPixels { getContextPixels :: a }
  deriving (Conjugate, Enum, Epsilon, Eq, Foldable, Floating, Fractional, Functor, Integral, Num, Ord, Real, RealFloat, RealFrac, Show, Storable, Traversable, GL.Type, Uniform)
  deriving (Additive, Applicative, Metric, Monad) via Identity

newtype WindowPixels a = WindowPixels { getWindowPixels :: a }
  deriving (Conjugate, Enum, Epsilon, Eq, Foldable, Floating, Fractional, Functor, Integral, Num, Ord, Real, RealFloat, RealFrac, Show, Storable, Traversable, GL.Type, Uniform)
  deriving (Additive, Applicative, Metric, Monad) via Identity

data ZoomedSpace a
data PlayerSpace a


toContext :: View -> Transform ClipSpace ContextPixels
toContext View{ size } = mkScale (pure 1 & _xy .~ 1 / (fromIntegral <$> size))

toWindow :: View -> Transform ContextPixels WindowPixels
toWindow View{ ratio } = mkScale (pure 1 & _xy .~ fromIntegral ratio)

toZoomed :: View -> Transform WindowPixels ZoomedSpace
toZoomed View{ zoom } = mkScale (pure 1 & _xy .~ pure (1/zoom))

toSystem :: View -> Transform ZoomedSpace SystemSpace
toSystem View{ scale, focus } = mkScale (pure scale) >>> mkTranslation (ext (prj <$> negated focus) 0)

toPlayer :: View -> Transform SystemSpace PlayerSpace
toPlayer View{ focus } = mkTranslation (ext (prj <$> focus) 0)

transformToWindow :: View -> Transform ClipSpace WindowPixels
transformToWindow view = toContext view >>> toWindow view

transformToZoomed :: View -> Transform ClipSpace ZoomedSpace
transformToZoomed view = transformToWindow view >>> toZoomed view

transformToSystem :: View -> Transform ClipSpace SystemSpace
transformToSystem view = transformToZoomed view >>> toSystem view


clipTo :: Has (Lift IO) sig m => View -> m ()
clipTo view = do
  viewport $ Interval 0 dsize
  scissor  $ Interval 0 dsize where
  dsize = deviceSize view
