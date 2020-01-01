{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Starlight.Weapon.Laser
( Beam(..)
, runLaser
, drawLaser
, Drawable
) where

import           Control.Carrier.Reader
import           Control.Effect.Finally
import           Control.Effect.Lift
import           Control.Effect.Profile
import           Data.Coerce (coerce)
import           Data.Functor.Identity
import           Data.Functor.Interval
import           GL.Array
import           GL.Program
import           Linear.Affine
import           Linear.V2
import           Starlight.View
import           Starlight.Weapon.Laser.Shader as Laser
import           UI.Colour
import qualified UI.Drawable as UI
import           Unit.Angle

data Beam = Beam
  { position :: Point V2 Float
  , angle    :: Radians Float
  , colour   :: Colour Float
  }
  deriving (Show)


runLaser
  :: ( Has Finally sig m
     , Has (Lift IO) sig m
     )
  => ReaderC Drawable m a
  -> m a
runLaser m = do
  program <- build Laser.shader
  array   <- load vertices
  runReader (Drawable UI.Drawable{ program, array }) m


drawLaser
  :: ( Has (Lift IO) sig m
     , Has Profile sig m
     , Has (Reader Drawable) sig m
     , Has (Reader View) sig m
     )
  => Beam
  -> m ()
drawLaser Beam{ colour, angle } = measure "laser" . UI.using getDrawable $ do
  matrix <- asks scaleToViewZoomed
  set Laser.U
    { matrix = Just matrix
    , angle  = Just angle
    , colour = Just colour
    }
  drawArrays Lines range


newtype Drawable = Drawable { getDrawable :: UI.Drawable U V O }


vertices :: [Laser.V Identity]
vertices = coerce @[Float] [0, 1]

range :: Interval Identity Int
range = Interval 0 (Identity (length vertices))
