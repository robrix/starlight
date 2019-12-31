{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Starlight.Weapon.Laser
( laser
, Laser
, drawLaser
) where

import Control.Effect.Finally
import Control.Effect.Lift
import Control.Effect.Profile
import Control.Effect.Reader
import Data.Coerce (coerce)
import Data.Functor.I
import Data.Functor.Interval
import GL.Array
import GL.Program
import Starlight.View
import Starlight.Weapon.Laser.Shader as Laser
import UI.Colour
import Unit.Angle

laser
  :: ( Has Finally sig m
     , Has (Lift IO) sig m
     )
  => m Laser
laser = do
  program <- build Laser.shader
  array <- load vertices
  pure Laser{ program, array }

drawLaser
  :: ( Has (Lift IO) sig m
     , Has Profile sig m
     , Has (Reader ViewScale) sig m
     )
  => Laser
  -> Colour Float
  -> Radians Float
  -> m ()
drawLaser Laser{ program, array } colour angle = measure "laser" . use program . bindArray array $ do
  matrix <- asks scaleToViewZoomed
  set Laser.U
    { matrix = Just matrix
    , angle  = Just angle
    , colour = Just colour
    }
  drawArrays Lines range


data Laser = Laser
  { program :: Program Laser.U Laser.V Laser.O
  , array   :: Array (Laser.V I)
  }

vertices :: [Laser.V I]
vertices = coerce @[Float] [0, 1]

range :: Interval I Int
range = Interval 0 (I (length vertices))
