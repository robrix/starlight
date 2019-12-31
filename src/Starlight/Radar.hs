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
import           Linear.Exts
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

  set defaultVars
    { Radar.matrix = Just (scaleToViewZoomed viewScale)
    , Radar.radius = Just 100
    }

  -- FIXME: skip blips for extremely distant objects
  -- FIXME: blips should shadow more distant blips
  let drawBodyBlip StateVectors{ scale, body = Body{ radius = Metres r, colour }, transform } = do
        let there = (transform !* V4 0 0 0 1) ^. _xy
            angle = angleTo here there
            d = distance here there
            radius = 100
            direction = normalize (there ^-^ here)
            edge = scale * r * (min d radius/d) *^ perp direction + direction ^* radius + here
            sweep = max minSweep (abs (wrap (Interval (-pi) pi) (angleTo here edge - angle)))

        set Radar.U
          { matrix = Nothing
          , radius = Nothing
          , angle  = Just angle
          , sweep  = Just sweep
          , colour = Just (colour & _a .~ 0.5)
          }

        drawArrays LineStrip (Interval 0 (I (length radarV)))

      drawNPCBlip Actor{ position = P there } = do
        set Radar.U
          { matrix = Nothing
          , radius = Nothing
          , angle  = Just $ angleTo here there
          , sweep  = Just 0
            -- FIXME: fade colour with distance
            -- FIXME: IFF
          , colour = Just white
          }
        let median = length radarV `div` 2
        drawArrays Points (Interval (I median) (I (median + 1)))

      drawTargetBlip StateVectors{ scale, body = Body{ radius = Metres r, colour }, transform } = do
        for_ [1..n] $ \ i -> do
          let there = (transform !* V4 0 0 0 1) ^. _xy
              angle = angleTo here there
              d = distance here there
              direction = normalize (there ^-^ here)
              -- drawAtRadius radius minSweep colour = do
              minSweep' = (minSweep * Radians (fromIntegral i / (zoom * 3)))
              radius = step * fromIntegral i
              -- FIXME: apply easing so this works more like a spring
              step = max 1 (min (50 * zoom) (d / fromIntegral n))
              edge = scale * r * (min d radius/d) *^ perp direction + direction ^* radius + here
              sweep = max minSweep' (abs (wrap (Interval (-pi) pi) (angleTo here edge - angle)))

          set Radar.U
            { matrix = Nothing
            , radius = Just radius
            , angle  = Just angle
            , sweep  = Just sweep
            , colour = Just ((colour + 0.5 * fromIntegral i / fromIntegral n) ** 2 & _a .~ (fromIntegral i / fromIntegral n))
            }

          drawArrays LineStrip (Interval 0 (I (length radarV)))


  for_ bodies drawBodyBlip
  for_ npcs drawNPCBlip
  for_ (target >>= \ i -> (bodies !! i) <$ guard (i < length bodies)) drawTargetBlip
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

-- | Return a matrix transforming the [[-1,1], [-1,1]] interval to zoomed device coordinates.
scaleToViewZoomed :: (Applicative v, Traversable v, R2 v) => ViewScale -> v (v Float)
scaleToViewZoomed ViewScale{ scale, size, zoom } = scaled (pure 1 & _xy .~ (1 / (fromIntegral <$> size) ^* fromIntegral scale ^* (1 / zoom)))


radarV :: [Radar.V I]
radarV = coerce @[Float] [ fromIntegral t / fromIntegral n | t <- [-n..n] ] where
  n = (16 :: Int)
