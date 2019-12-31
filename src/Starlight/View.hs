{-# LANGUAGE NamedFieldPuns #-}
module Starlight.View
( ViewScale(..)
, scaleToViewZoomed
) where

import Data.Function ((&))
import Lens.Micro ((.~))
import Linear.V2
import Linear.Vector

data ViewScale = ViewScale
  { scale :: Int
  , size  :: V2 Int
  , zoom  :: Float
  }

-- | Return a matrix transforming the [[-1,1], [-1,1]] interval to zoomed device coordinates.
scaleToViewZoomed :: (Applicative v, Traversable v, R2 v) => ViewScale -> v (v Float)
scaleToViewZoomed ViewScale{ scale, size, zoom } = scaled (pure 1 & _xy .~ (1 / (fromIntegral <$> size) ^* fromIntegral scale ^* (1 / zoom)))
