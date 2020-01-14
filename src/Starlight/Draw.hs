{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module Starlight.Draw
( Starlight.Draw.draw
) where

import Control.Effect.Lens (view)
import Control.Effect.Lift
import Control.Effect.Profile
import Control.Effect.Reader
import Control.Lens (filtered, traversed, (^.), (^?))
import Control.Monad.IO.Class.Lift
import Data.Foldable (for_)
import Data.Functor.Const
import Data.Functor.Identity
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
import Starlight.System
import Starlight.UI
import Starlight.View
import UI.Colour
import UI.Label
import UI.Typeface
import Unit.Algebra
import Unit.Length
import Unit.Time

draw
  :: ( Has Check sig m
     , Has (Lift IO) sig m
     , Has Profile sig m
     , Has (Reader Body.Drawable) sig m
     , Has (Reader Laser.Drawable) sig m
     , Has (Reader Radar.Drawable) sig m
     , Has (Reader Ship.Drawable) sig m
     , Has (Reader Starfield.Drawable) sig m
     , Has (Reader (Seconds Float)) sig m
     , Has (Reader (System StateVectors)) sig m
     , Has (Reader UI) sig m
     , Has (Reader View) sig m
     )
  => m ()
draw = measure "draw" . runLiftIO $ do
  dt <- ask @(Seconds Float)
  UI{ fps = fpsLabel, target = targetLabel, face } <- ask
  let font = Font face 18
  bind @Framebuffer Nothing

  v@View{ size } <- ask
  system@System{ beams } <- ask @(System StateVectors)
  Character{ actor = Actor{ position }, target } <- view (player_ @StateVectors)

  clipTo v

  glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA

  measure "starfield" drawStarfield

  measure "ship" $ for_ (system^.characters_) drawShip

  measure "laser" $ for_ beams drawLaser

  let maxDim = maximum (fromIntegral <$> size) * 0.5
      onScreen StateVectors{ body = Body{ radius }, actor = Actor{ position = pos } } = lengthToPixels v * prj (distance pos position - convert radius) < maxDim

  measure "body" $ for_ (system^?bodies_.traversed.filtered onScreen) Body.draw

  measure "radar" drawRadar

  let describeTarget target = case target >>= fmap . (,) <*> (system !?) of
        Just (identifier, t)
          | pos <- either Body.actor Character.actor t ^. position_ -> describeIdentifier identifier ++ ": " ++ formatExp (Just 1) (convert @_ @(Kilo Metres) (distance pos position))
        _ -> ""

  measure "setLabel" $ setLabel fpsLabel    font (formatDec (Just 1) (nu @(Milli Seconds) dt) <> "/" <> formatDec (Just 1) (nu @(Frames :/: Seconds) (1/dt)))
  measure "setLabel" $ setLabel targetLabel font (describeTarget target)

  fpsSize <- labelSize fpsLabel
  measure "drawLabel" $ drawLabel fpsLabel    (V2 10 (size^._y - fpsSize^._y - 10)) white Nothing
  measure "drawLabel" $ drawLabel targetLabel (V2 10 10)                            white Nothing

newtype Frames a = Frames a
  deriving (Eq, Floating, Fractional, Functor, Num, Ord, Real, RealFloat, RealFrac)
  deriving Applicative via Identity

instance Unit Rate Frames where suffix = Const ('f':)

data Rate a
