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
import Data.Foldable (for_)
import Data.Functor.Interval
import GL.Framebuffer
import GL.Viewport
import Graphics.GL.Core41
import Lens.Micro
import Linear.Exts
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
     , Has (Reader (System StateVectors)) sig m
     , Has (Reader View) sig m
     )
  => Delta Seconds Float
  -> Label
  -> Label
  -> Font
  -> Player
  -> m ()
draw dt fpsLabel targetLabel font player = measure "draw" . runLiftIO $ do
  let Actor{ position, rotation, target } = player ^. actor_
  bind @Framebuffer Nothing

  v@View{ size, zoom } <- ask

  let dsize = deviceSize v

  viewport $ Interval 0 dsize
  scissor  $ Interval 0 dsize

  glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA

  drawStarfield

  system@System{ scale, actors = npcs } <- ask @(System StateVectors)

  for_ (player ^. actor_ : npcs) (drawShip white)

  when False $ drawLaser Beam { colour = green, angle = snd (toAxisAngle rotation), position }

  let maxDim = maximum (fromIntegral <$> dsize) * zoom
      onScreen StateVectors{ body = Body{ radius }, position = pos } = distance pos position - scale * getMetres radius < maxDim * 0.5

  forOf_ (bodies_ . traversed) system $ \ sv -> when (onScreen sv) (drawBody sv)

  drawRadar (player ^. actor_)

  let rscale = 1/scale
      describeTarget target = case target >>= fmap . (,) <*> (system !?) of
        Just (identifier, t)
          | pos <- either (^. position_) (^. position_) t -> describeIdentifier identifier ++ ": " ++ showEFloat (Just 1) (kilo (Metres (distance (pos ^* rscale) (position ^* rscale)))) "km"
        _ -> ""

  measure "setLabel" $ setLabel fpsLabel    font (showFFloat (Just 1) (dt * 1000) "ms/" <> showFFloat (Just 1) (1/dt) "fps")
  measure "setLabel" $ setLabel targetLabel font (describeTarget target)

  fpsSize <- labelSize fpsLabel
  measure "drawLabel" $ drawLabel fpsLabel    (V2 10 (size ^. _y - fpsSize ^. _y - 10)) white Nothing
  measure "drawLabel" $ drawLabel targetLabel (V2 10 10)                                white Nothing
