{-# LANGUAGE NamedFieldPuns #-}
module Starlight.View
( View(..)
, scaleToView
, scaleToViewZoomed
, scaleToViewSystem
, aspectRatio
, deviceSize
, lengthToPixels
  -- * Transforms
, Transform(..)
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
  -- * Re-exports
, (>>>)
) where

import Control.Category
import Control.Lens ((.~), (^.))
import Data.Function ((&))
import Linear.Exts
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

-- | Return a matrix transforming the [[-1,1], [-1,1]] interval to zoomed device coordinates.
scaleToViewZoomed :: (Additive v, Applicative v, Traversable v, R2 v) => View -> v (v Float)
scaleToViewZoomed vs@View{ zoom } = scaleToView vs !*! scaled (pure 1 & _xy .~ pure (1 / zoom))

-- | Return a matrix transforming the [[-1,1], [-1,1]] interval to zoomed, scaled device coordinates.
scaleToViewSystem :: (Additive v, Applicative v, Traversable v, R3 v) => View -> v (v Float)
scaleToViewSystem view@View{ scale } = scaleToViewZoomed view !*! scaled (pure 1 & _xyz .~ pure scale)

aspectRatio :: View -> Float
aspectRatio View{ size } = size'^._x / size'^._y where
  size' = fromIntegral <$> size

deviceSize :: View -> V2 Int
deviceSize View{ ratio, size } = ratio *^ size

lengthToPixels :: View -> Float
lengthToPixels View{ zoom, scale } = 1/zoom * scale


newtype Transform a b = Transform { getTransform :: M44 Float }

instance Category Transform where
  id = Transform identity
  Transform a . Transform b = Transform (b !*! a)

data ClipSpace
data ContextSpace
data WindowSpace
data ZoomedSpace
data SystemSpace
data PlayerSpace


toContext :: View -> Transform ClipSpace ContextSpace
toContext View{ size } = Transform (scaled (pure 1 & _xy .~ 1 / (fromIntegral <$> size)))

toWindow :: View -> Transform ContextSpace WindowSpace
toWindow View{ ratio } = Transform (scaled (pure 1 & _xy .~ fromIntegral ratio))

toZoomed :: View -> Transform WindowSpace ZoomedSpace
toZoomed View{ zoom } = Transform (scaled (pure 1 & _xy .~ pure (1/zoom)))

toSystem :: View -> Transform ZoomedSpace SystemSpace
toSystem View{ scale } = Transform (scaled (pure 1 & _xyz .~ pure scale))

toPlayer :: View -> Transform SystemSpace PlayerSpace
toPlayer View{ focus } = Transform (translated3 (ext (prj <$> negated focus) 0))
