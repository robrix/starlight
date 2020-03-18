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
import Control.Carrier.State.Church
import Control.Effect.Finally
import Control.Effect.Lift
import Control.Effect.Profile
import Control.Effect.Trace
import Control.Lens (choosing, filtered, forOf_, traversed, (^.))
import Control.Monad.IO.Class.Lift
import Data.Foldable (for_)
import Data.Maybe (fromMaybe)
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

runFrame
  :: ( Has Check sig m
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
     )
  => m ()
frame = runSystem $ do
  measure "input" Starlight.Input.input
  withView . local (neighbourhoodOfPlayer @StateVectors) . measure "draw" . runLiftIO $ do
    UI{ target, face } <- ask
    let font = Font face 18
    bind @Framebuffer Nothing

    v@View{ size } <- ask
    system <- ask @(System StateVectors)

    let hypotenuse = norm (fromIntegral <$> size) * 0.5
        onScreen a = lengthToWindowPixels v .*. (distance (a^.position_) (system^.player_.position_) - a^.magnitude_ * 0.5) < hypotenuse

    clipTo v

    glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA

    measure "starfield" Starfield.draw

    measure "ship" $ forOf_ (characters_.traversed.filtered onScreen) system Ship.draw

    measure "laser" $ for_ (beams system) Laser.draw

    measure "body" $ forOf_ (bodies_.traversed.filtered onScreen) system Body.draw

    measure "radar" Radar.draw

    measure "setLabel" . setLabel target font . fromMaybe "" $ do
      identifier <- system^.player_.target_
      pos <- (^.choosing position_ position_) <$> system !? identifier
      pure $! describeIdentifier identifier ++ ": " ++ formatExpR (Just 1) (convert @_ @(Kilo Metres) (distance pos (system^.player_.position_)))
    measure "drawLabel" $ drawLabel target 10 white Nothing
