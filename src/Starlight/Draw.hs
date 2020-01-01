{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Starlight.Draw
( draw
) where

import Control.Effect.Lift
import Control.Effect.Profile
import Control.Effect.Reader
import Control.Monad (when)
import Control.Monad.IO.Class.Lift
import Data.Foldable (find, for_)
import Data.Functor.Interval
import GL.Framebuffer
import GL.Viewport
import Graphics.GL.Core41
import Lens.Micro
import Linear.Exts
import Linear.Metric
import Linear.V2
import Linear.Vector
import Numeric
import Starlight.Actor
import Starlight.Body as Body
import Starlight.Identifier
import Starlight.Player
import Starlight.Radar as Radar
import Starlight.Ship as Ship
import Starlight.Starfield as Starfield
import Starlight.System
import Starlight.View
import Starlight.Weapon.Laser as Laser
import UI.Colour
import UI.Label
import UI.Typeface
import Unit.Length
import Unit.Time

draw
  :: ( Has (Lift IO) sig m
     , Has Profile sig m
     , Has (Reader Body.Drawable) sig m
     , Has (Reader Laser.Drawable) sig m
     , Has (Reader Radar.Drawable) sig m
     , Has (Reader Ship.Drawable) sig m
     , Has (Reader Starfield.Drawable) sig m
     , Has (Reader (System StateVectors Float)) sig m
     , Has (Reader View) sig m
     )
  => Delta Seconds Float
  -> Label
  -> Label
  -> Font
  -> Player
  -> [Actor]
  -> m ()
draw dt fpsL targetL font player npcs = measure "draw" . runLiftIO $ do
  let Actor{ position, rotation, target } = player ^. actor_
  bind @Framebuffer Nothing

  View{ scale, size, zoom } <- ask

  viewport $ scale *^ Interval 0 size
  scissor  $ scale *^ Interval 0 size

  glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA

  drawStarfield

  for_ (player ^. actor_ : npcs) (drawShip white)

  when (player ^. firing_) $ drawLaser green (snd (toAxisAngle rotation))

  let maxDim = maximum (fromIntegral <$> size ^* scale) * zoom

  System{ scale, bodies } <- ask @(System StateVectors Float)

  let onScreen StateVectors{ body = Body{ radius }, position = pos } = distance pos position - scale * getMetres radius < maxDim * 0.5

  for_ bodies $ \ sv -> when (onScreen sv) (drawBody sv)

  drawRadar (player ^. actor_) npcs

  let describeTarget target = case target >>= \ i -> find ((== i) . identifier . Body.body) bodies of
        Just StateVectors{ body, position = pos } -> describeIdentifier (identifier body) ++ ": " ++ showEFloat (Just 1) (kilo (Metres (distance (pos ^* (1/scale)) (position ^* (1/scale))))) "km"
        _ -> ""

  measure "setLabel" $ setLabel fpsL    font (showFFloat (Just 1) (dt * 1000) "ms/" <> showFFloat (Just 1) (1/dt) "fps")
  measure "setLabel" $ setLabel targetL font (describeTarget target)

  fpsSize <- labelSize fpsL
  measure "drawLabel" $ drawLabel fpsL    (V2 10 (size ^. _y - fpsSize ^. _y - 10)) white Nothing
  measure "drawLabel" $ drawLabel targetL (V2 10 10) white Nothing
