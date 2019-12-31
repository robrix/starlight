{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}
module Starlight.Radar
( radar
, drawRadar
, Radar
, ViewScale(..)
, scaleToViewZoomed
) where

import           Control.Effect.Finally
import           Control.Effect.Lift
import           Control.Effect.Reader
import           Control.Monad (guard)
import           Data.Coerce (coerce)
import           Data.Foldable (for_)
import           Data.Function ((&))
import           Data.Functor.I
import           Data.Functor.Interval
import           GL.Array
import           GL.Program
import           GL.Shader.DSL (defaultVars)
import           Lens.Micro ((.~), (^.))
import           Linear.Affine
import           Linear.Exts as Linear
import           Linear.Matrix
import           Linear.Metric
import           Linear.V2
import           Linear.V4
import           Linear.Vector
import           Starlight.Actor
import           Starlight.Body as Body
import qualified Starlight.Radar.Shader as Radar
import           UI.Colour
import           Unit.Angle
import           Unit.Length

radar :: (Has Finally sig m, Has (Lift IO) sig m) => m Radar
radar = do
  radarP <- build Radar.shader
  radarA <- load radarV
  pure Radar { radarP, radarA }


drawRadar
  :: ( Has (Lift IO) sig m
     , Has (Reader [StateVectors Float]) sig m
     , Has (Reader ViewScale) sig m
     )
  => Radar
  -> Actor
  -> [Actor]
  -> m ()
drawRadar Radar{ radarA, radarP } Actor{ position = P here, target } npcs = use radarP . bindArray radarA $ do
  bodies <- ask
  viewScale@ViewScale{ zoom } <- ask

  let radius = 100
  set defaultVars
    { Radar.matrix = Just (scaleToViewZoomed viewScale)
    , Radar.radius = Just radius
    }

  -- FIXME: skip blips for extremely distant objects
  -- FIXME: blips should shadow more distant blips
  for_ bodies $ \ StateVectors{ scale, body = Body{ radius = Metres r, colour }, transform } -> do
    setBlip (makeBlip here ((transform !* V4 0 0 0 1) ^. _xy) (r * scale) colour)
    drawArrays LineStrip (Interval 0 (I (length radarV)))

  set defaultVars
    { Radar.sweep  = Just 0
      -- FIXME: fade colour with distance
      -- FIXME: IFF
    , Radar.colour = Just white
    }
  for_ npcs $ \ Actor{ position = P there } -> do
    set defaultVars { Radar.angle  = Just $ angleTo here there }
    let median = length radarV `div` 2
    drawArrays Points (Interval (I median) (I (median + 1)))

  let targetVectors = target >>= \ i -> (bodies !! i) <$ guard (i < length bodies)
  for_ targetVectors $ \ StateVectors{ scale, body = Body{ radius = Metres r, colour }, transform } -> do
    let blip = makeBlip here ((transform !* V4 0 0 0 1) ^. _xy) (r * scale) colour
    setBlip blip
    for_ [1..n] $ \ i -> do
      let radius = step * fromIntegral i
          -- FIXME: apply easing so this works more like a spring
          step = max 1 (min (50 * zoom) (d blip / fromIntegral n))

      set defaultVars
        { Radar.radius = Just radius
        , Radar.colour = Just ((colour + 0.5 * fromIntegral i / fromIntegral n) ** 2 & _a .~ fromIntegral i / fromIntegral n)
        }

      drawArrays LineStrip (Interval 0 (I (length radarV)))
  where
  n = 10 :: Int

setBlip
  :: ( Has (Lift IO) sig m
     , HasProgram Radar.U Radar.V Radar.O m
     )
  => Blip
  -> m ()
setBlip Blip{ angle, direction, d, r, colour } = do
  set defaultVars
    { Radar.angle  = Just angle
    , Radar.sweep  = Just sweep
    , Radar.colour = Just colour
    }
  where
  edge = perp direction ^* r + direction ^* d
  sweep = max minSweep (abs (wrap (Interval (-pi) pi) (angleOf edge - angle)))
  minSweep = 0.0133 -- at radius'=150, makes approx. 4px blips

data Blip = Blip
  { angle     :: Radians Float -- ^ angle to the object
  , d         :: Float         -- ^ distance to the object
  , direction :: V2 Float      -- ^ unit vector in the direction of the object
  , r         :: Float         -- ^ magnitude of the object
  , colour    :: Colour Float  -- ^ colour of the object
  }

makeBlip :: V2 Float -> V2 Float -> Float -> Colour Float -> Blip
makeBlip here there r colour = Blip { angle, d, direction, r, colour } where
  angle = angleTo here there
  d = distance here there
  direction = Linear.direction there here


data Radar = Radar
  { radarP :: Program Radar.U Radar.V Radar.O
  , radarA :: Array (Radar.V I)
  }

data ViewScale = ViewScale
  { scale :: Int
  , size  :: V2 Int
  , zoom  :: Float
  }

-- | Return a matrix transforming the [[-1,1], [-1,1]] interval to zoomed device coordinates.
scaleToViewZoomed :: (Applicative v, Traversable v, R2 v) => ViewScale -> v (v Float)
scaleToViewZoomed ViewScale{ scale, size, zoom } = scaled (pure 1 & _xy .~ (1 / (fromIntegral <$> size) ^* fromIntegral scale ^* (1 / zoom)))


radarV :: [Radar.V I]
radarV = coerce @[Float] [ fromIntegral t / fromIntegral n | t <- [-n..n] ] where
  n = (16 :: Int)
