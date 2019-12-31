{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}
module Starlight.Radar
( radar
, drawRadar
, Radar
, ViewScale(..)
) where

import           Control.Effect.Finally
import           Control.Effect.Lift
import           Control.Effect.Reader
import           Control.Monad (guard, when)
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
import           Linear.Exts
import           Linear.Matrix
import           Linear.Metric
import           Linear.V2
import           Linear.V3
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
  ViewScale{ scale, size, zoom = zoomOut } <- ask
  let V2 sx sy = 1 / (fromIntegral <$> size) ^* fromIntegral scale ^* (1 / zoomOut)

  set defaultVars
    { Radar.matrix = Just (scaled (V3 sx sy 1))
    }

  -- FIXME: skip blips for extremely distant objects
  let drawBodyBlip StateVectors{ scale, body = Body{ name, radius = Metres r, colour }, transform } = do
        let there = (transform !* V4 0 0 0 1) ^. _xy
            angle = angleTo here there
            d = distance here there
            direction = normalize (there ^-^ here)
            -- FIXME: apply easing so this works more like a spring
            step = max 1 (min (50 * zoomOut) (d / fromIntegral n))
            drawAtRadius radius minSweep colour = do
              let edge = scale * r * (min d radius/d) *^ perp direction + direction ^* radius + here
                  sweep = max minSweep (abs (wrap (Interval (-pi) pi) (angleTo here edge - angle)))

              set Radar.U
                { matrix = Nothing
                , radius = Just radius
                , angle  = Just angle
                , sweep  = Just sweep
                , colour = Just colour
                }

              drawArrays LineStrip (Interval 0 (I (length radarV)))

        drawAtRadius 100 minSweep (colour & _a .~ 0.5)

        when (Just name == (target >>= \ i -> Body.name (body (bodies !! i)) <$ guard (i < length bodies))) $ for_ [1..n] $ \ i ->
          drawAtRadius (step * fromIntegral i) (minSweep * Radians (fromIntegral i / (zoomOut * 3))) ((colour + 0.5 * fromIntegral i / fromIntegral n) ** 2 & _a .~ (fromIntegral i / fromIntegral n))

      drawNPCBlip Actor{ position = P there } = do
        set Radar.U
          { matrix = Nothing
          , radius = Just 100
          , angle  = Just $ angleTo here there
          , sweep  = Just 0
            -- FIXME: fade colour with distance
          , colour = Just white
          }
        let median = length radarV `div` 2
        drawArrays Points (Interval (I median) (I (median + 1)))

  for_ bodies drawBodyBlip
  for_ npcs drawNPCBlip
  where
  n = 10 :: Int
  minSweep = 0.0133 -- at d=150, makes approx. 4px blips

data Radar = Radar
  { radarP :: Program Radar.U Radar.V Radar.O
  , radarA :: Array (Radar.V I)
  }

data ViewScale = ViewScale
  { scale :: Int
  , size  :: V2 Int
  , zoom  :: Float
  }

radarV :: [Radar.V I]
radarV = coerce @[Float] [ fromIntegral t / fromIntegral n | t <- [-n..n] ] where
  n = (16 :: Int)
