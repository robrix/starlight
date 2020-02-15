{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module Starlight.View
( View(..)
, contextSize
, lengthToWindowPixels
, zoomForSpeed
, withView
  -- * Transforms
, ClipUnits(..)
, transformToWindow
, transformToZoomed
, transformToSystem
  -- * Viewport
, clipTo
  -- * Re-exports
, module Geometry.Transform
) where

import Control.Carrier.Reader
import Control.Effect.Lens (view)
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
import Starlight.Actor
import Starlight.Body
import Starlight.Physics
import Starlight.System
import System.Random (Random)
import UI.Context as Context
import UI.Window as Window
import Unit.Algebra
import Unit.Length
import Unit.Time

data View = View
  { ratio     :: I Int    -- ^ Ratio of window pixels per context pixel.
  , size      :: V2 (Window.Coords Int)
  , zoom      :: I Double
  , scale     :: (Window.Coords :/: Distance) Double
  , shipScale :: I Double
  , focus     :: V2 (Distance Double)
  }

contextSize :: View -> V2 (Context.Pixels Int)
contextSize View{ ratio, size } = Context.Pixels . Window.getCoords <$> ratio .*^ size

lengthToWindowPixels :: View -> (Window.Coords :/: Distance) Double
lengthToWindowPixels View{ zoom, scale } = scale .*. zoom

-- | Compute the zoom factor for the given velocity.
--
-- Higher values correlate to more of the scene being visible.
zoomForSpeed :: V2 (Window.Coords Int) -> (Distance :/: Seconds) Double -> I Double
zoomForSpeed size x
  | distance < inf bounds = inf zoom
  | distance > sup bounds = sup zoom
  | otherwise             = fromUnit zoom (coerce easeInOutCubic (toUnit bounds distance))
  where
  hypotenuse = norm (fmap fromIntegral <$> size)
  distance = I (convert @Distance @(Mega Metres) (x .*. Seconds 1) ./. hypotenuse) -- how much of the screen will be traversed in a second
  zoom = 1...1/5
  bounds = (1...(20 :: Mega Metres Double)) ^/. hypotenuse

withView
  :: ( Has (Lift IO) sig m
     , Has (Reader (System StateVectors)) sig m
     , Has (Reader Window.Window) sig m
     )
  => ReaderC View m a
  -> m a
withView m = do
  ratio <- Window.ratio
  size  <- Window.size

  velocity <- view (player_ @StateVectors .velocity_)
  focus    <- view (player_ @StateVectors .position_._xy)

  let zoom = zoomForSpeed size (norm velocity)
      -- how many pixels to draw something / the radius of the sun
      scale = Window.Coords 695_500 ./. convert @(Kilo Metres) @Distance 695_500.0
      -- FIXME: this is really stupid; there *has* to be a better way to say “I want a 500 m ship to be 30 px long” or w/e
      shipScale = 30

  runReader View{ ratio, size, zoom, scale, shipScale, focus } m


newtype ClipUnits a = ClipUnits { getClipUnits :: a }
  deriving (Column, Conjugate, Enum, Epsilon, Eq, Foldable, Floating, Fractional, Functor, Integral, Num, Ord, Random, Real, RealFloat, RealFrac, Row, Show, Storable, Traversable, GL.Type, Uniform)
  deriving (Additive, Applicative, Metric, Monad) via I

instance Unit Length ClipUnits where
  suffix = K ("clip"++)


transformToWindow :: View -> Transform Double ClipUnits Window.Coords
transformToWindow View{ size }
  = mkScale (pure 1 & _xy .~ ClipUnits 1 ./^ (fmap fromIntegral <$> size))

transformToZoomed :: View -> Transform Double ClipUnits Window.Coords
transformToZoomed view@View{ zoom }
  =   transformToWindow view
  >>> mkScale (pure zoom)

transformToSystem :: View -> Transform Double ClipUnits Distance
transformToSystem view@View{ scale, focus }
  =   transformToZoomed view
  >>> mkScale (pure scale)
  >>> mkTranslation (ext (negated focus) 0)


clipTo :: Has (Lift IO) sig m => View -> m ()
clipTo view = do
  let dsize = contextSize view
  viewport $ Interval 0 dsize
  scissor  $ Interval 0 dsize
