{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Starlight.Radar
( radar
, drawRadar
, Radar
) where

import           Control.Effect.Finally
import           Control.Effect.Lift
import           Control.Effect.Reader
import           Control.Effect.Profile
import           Control.Monad (when)
import           Data.Coerce (coerce)
import           Data.Foldable (for_)
import           Data.Function ((&))
import           Data.Functor.I
import           Data.Functor.Interval
import           Data.List (find)
import           GL.Array
import           GL.Program
import           GL.Shader.DSL (defaultVars)
import           Lens.Micro ((.~))
import           Linear.Affine
import           Linear.Exts as Linear
import           Linear.Metric
import           Linear.V2
import           Linear.Vector
import           Starlight.Actor
import           Starlight.Body as Body
import qualified Starlight.Radar.Shader as Radar
import           Starlight.System
import           Starlight.View
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
     , Has Profile sig m
     , Has (Reader (System StateVectors Float)) sig m
     , Has (Reader ViewScale) sig m
     )
  => Radar
  -> Actor
  -> [Actor]
  -> m ()
drawRadar Radar{ radarA, radarP } Actor{ position = P here, target } npcs = measure "radar" . use radarP . bindArray radarA $ do
  System{ scale, bodies } <- ask @(System StateVectors Float)
  vs <- ask

  let radius = 100
  set defaultVars
    { Radar.matrix = Just (scaleToView vs)
    , Radar.radius = Just radius
    }

  -- FIXME: skip blips for extremely distant objects
  -- FIXME: blips should shadow more distant blips
  measure "bodies" $
    for_ bodies $ \ StateVectors{ body = Body{ radius = Metres r, colour }, position = there } -> do
      setBlip (makeBlip (there ^-^ here) (r * scale) colour)
      drawArrays LineStrip (Interval 0 (I vertexCount))

  measure "npcs" $ do
    set defaultVars
      { Radar.sweep  = Just 0
        -- FIXME: fade colour with distance
        -- FIXME: IFF
      , Radar.colour = Just white
      }
    for_ npcs $ \ Actor{ position = P there } -> let (angle, r) = polar2 (there ^-^ here) in when (r > zoom vs * radius) $ do
      set defaultVars { Radar.angle  = Just angle }
      drawArrays Points (Interval (I medianVertex) (I (medianVertex + 1)))

  measure "targets" $ do
    let targetVectors = target >>= \ i -> find ((== i) . identifier . body) bodies
    for_ targetVectors $ \ StateVectors{ body = Body{ radius = Metres r, colour }, position = there } -> do
      let blip = makeBlip (there ^-^ here) (r * scale) colour
      setBlip blip
      for_ [1..n] $ \ i -> do
        let radius = step * fromIntegral i
            -- FIXME: apply easing so this works more like a spring
            step = max 1 (min 50 (d blip / fromIntegral n))

        set defaultVars
          { Radar.radius = Just radius
          , Radar.colour = Just ((colour + 0.5 * fromIntegral i / fromIntegral n) ** 2 & _a .~ fromIntegral i / fromIntegral n)
          }

        drawArrays LineStrip (Interval 0 (I vertexCount))
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

makeBlip :: V2 Float -> Float -> Colour Float -> Blip
makeBlip there r colour = Blip { angle, d, direction, r, colour } where
  angle = angleOf there
  d = norm there
  direction = normalize there


data Radar = Radar
  { radarP :: Program Radar.U Radar.V Radar.O
  , radarA :: Array (Radar.V I)
  }


radarV :: [Radar.V I]
radarV = coerce @[Float] [ fromIntegral t / fromIntegral n | t <- [-n..n] ] where
  n = (16 :: Int)

vertexCount :: Int
vertexCount = length radarV

medianVertex :: Int
medianVertex = vertexCount `div` 2
