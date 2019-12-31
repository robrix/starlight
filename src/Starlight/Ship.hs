{-# LANGUAGE RankNTypes #-}
module Starlight.Ship
( DrawShip
, drawShip
) where

import Control.Effect.Finally
import Control.Effect.Lift
import Control.Effect.Profile
import Control.Effect.Reader
import Starlight.Actor
import Starlight.View
import UI.Colour

newtype DrawShip = DrawShip
  { drawShip
    :: forall sig m
    .  ( Has (Lift IO) sig m
       , Has Profile sig m
       , Has (Reader ViewScale) sig m
       )
    => Colour Float
    -> Actor
    -> m ()
  }
