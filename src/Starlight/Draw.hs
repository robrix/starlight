{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Starlight.Draw
( runFrame
, frame
) where

import Control.Carrier.Empty.Church
import Control.Carrier.Reader
import Control.Carrier.State.Strict
import Control.Effect.Finally
import Control.Effect.Lens (view)
import Control.Effect.Lift
import Control.Effect.Profile
import Control.Effect.Trace
import Control.Lens (choosing, filtered, to, traversed, (^.), (^?), _Just)
import Control.Monad.IO.Class.Lift
import Data.Foldable (for_)
import Data.Time.Clock
import GL.Effect.Check
import GL.Framebuffer
import Graphics.GL.Core41
import Linear.Exts
import Starlight.Actor
import Starlight.Body as Body
import Starlight.Character as Character
import Starlight.Draw.Body as Body
import Starlight.Draw.Radar as Radar
import Starlight.Draw.Ship as Ship
import Starlight.Draw.Starfield as Starfield
import Starlight.Draw.Weapon.Laser as Laser
import Starlight.Identifier
import Starlight.Input
import Starlight.System
import Starlight.Time
import Starlight.UI
import Starlight.View
import UI.Colour
import UI.Label
import UI.Typeface
import UI.Window as Window
import Unit.Algebra
import Unit.Count
import Unit.Length
import Unit.Time

runFrame
  :: ( Effect sig
     , Has Check sig m
     , Has Finally sig m
     , Has (Lift IO) sig m
     , Has Trace sig m
     )
  => ReaderC Body.Drawable (ReaderC Laser.Drawable (ReaderC Radar.Drawable (ReaderC Ship.Drawable (ReaderC Starfield.Drawable (StateC UTCTime (EmptyC m)))))) a
  -> m ()
runFrame = evalEmpty . (\ m -> now >>= \ start -> evalState start m) . Starfield.run . Ship.run . Radar.run . Laser.run . Body.run

frame
  :: ( Has Check sig m
     , Has Empty sig m
     , Has (Lift IO) sig m
     , Has Profile sig m
     , Has (Reader Body.Drawable) sig m
     , Has (Reader Laser.Drawable) sig m
     , Has (Reader Radar.Drawable) sig m
     , Has (Reader Ship.Drawable) sig m
     , Has (Reader Starfield.Drawable) sig m
     , Has (Reader Epoch) sig m
     , Has (Reader UI) sig m
     , Has (Reader Window.Window) sig m
     , Has (State Input) sig m
     , Has (State (System Body)) sig m
     , Has (State UTCTime) sig m
     )
  => m ()
frame = runSystem . timed $ do
  measure "input" Starlight.Input.input
  withView (local (neighbourhoodOfPlayer @StateVectors) Starlight.Draw.draw) -- draw with current readonly positions & beams

draw
  :: ( Has Check sig m
     , Has (Lift IO) sig m
     , Has Profile sig m
     , Has (Reader Body.Drawable) sig m
     , Has (Reader Laser.Drawable) sig m
     , Has (Reader Radar.Drawable) sig m
     , Has (Reader Ship.Drawable) sig m
     , Has (Reader Starfield.Drawable) sig m
     , Has (Reader (Seconds Double)) sig m
     , Has (Reader (System StateVectors)) sig m
     , Has (Reader UI) sig m
     , Has (Reader View) sig m
     )
  => m ()
draw = measure "draw" . runLiftIO $ do
  dt <- ask @(Seconds Double)
  UI{ fps = fpsLabel, target = targetLabel, face } <- ask
  let font = Font face 18
  bind @Framebuffer Nothing

  v@View{ size } <- ask
  system@System{ beams } <- ask @(System StateVectors)
  Character{ actor = Actor{ position }, target } <- view (player_ @StateVectors)

  clipTo v

  glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA

  measure "starfield" Starfield.draw

  measure "ship" $ for_ (system^.characters_) Ship.draw

  measure "laser" $ for_ beams Laser.draw

  let hypotenuse = norm (fromIntegral <$> size) * 0.5
      onScreen StateVectors{ body = Body{ radius }, actor = Actor{ position = pos } } = lengthToWindowPixels v .*. (distance pos position - convert radius) < hypotenuse

  measure "body" $ for_ (system^?bodies_.traversed.filtered onScreen) Body.draw

  measure "radar" Radar.draw

  let describeTarget target = case target >>= fmap . (,) <*> (^?to (system !?)._Just.choosing position_ position_) of
        Just (identifier, pos)
          -> describeIdentifier identifier ++ ": " ++ formatExpR (Just 1) (convert @_ @(Kilo Metres) (distance pos position))
        _ -> ""

  measure "setLabel" $ setLabel fpsLabel    font (formatDec (Just 1) (convert @_ @(Milli Seconds) dt) <> "/" <> formatDec (Just 1) (Count @"f" 1 ./. dt))
  measure "setLabel" $ setLabel targetLabel font (describeTarget target)

  fpsSize <- labelSize fpsLabel
  measure "drawLabel" $ drawLabel fpsLabel    (V2 10 (size^._y - fpsSize^._y - 10)) white Nothing
  measure "drawLabel" $ drawLabel targetLabel (V2 10 10)                            white Nothing
