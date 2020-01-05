{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
module Starlight.Draw.Ship
( drawShip
, runShip
, Drawable
) where

import           Control.Carrier.Reader
import           Control.Effect.Finally
import           Control.Effect.Lens ((?=))
import           Control.Effect.Lift
import           Control.Effect.Profile
import           Data.Coerce (coerce)
import           Data.Functor.Identity
import           Data.Functor.Interval
import           GL.Array
import           GL.Effect.Check
import           GL.Program
import           Linear.Exts
import           Starlight.Actor
import           Starlight.Draw.Ship.Shader
import           Starlight.Ship hiding (colour_)
import           Starlight.View
import qualified UI.Drawable as UI

drawShip
  :: ( Has Check sig m
     , Has (Lift IO) sig m
     , Has Profile sig m
     , Has (Reader Drawable) sig m
     , Has (Reader View) sig m
     )
  => Ship
  -> m ()
drawShip Ship{ colour, actor = Actor{ position, rotation } } = measure "ship" . UI.using getDrawable $ do
  vs@View{ focus } <- ask
  let matrix = scaleToViewZoomed vs
  matrix_ ?=
        matrix
    !*! translated3 (ext (negated (unP focus)) 0)
    !*! translated3 (unP position)
    !*! scaled (V4 15 15 15 1)
    !*! mkTransformation rotation 0
  colour_ ?= colour
  drawArrays LineLoop range


runShip
  :: ( Has Check sig m
     , Has Finally sig m
     , Has (Lift IO) sig m
     , Effect sig
     )
  => ReaderC Drawable m a
  -> m a
runShip m = do
  program <- build shader
  array   <- load vertices
  runReader (Drawable UI.Drawable{ program, array }) m


newtype Drawable = Drawable { getDrawable :: UI.Drawable U V O }


vertices :: [V Identity]
vertices = coerce @[V2 Float]
  [ V2 1      0
  , V2 0      (-0.5)
  , V2 (-0.5) 0
  , V2 0      0.5
  ]

range :: Interval Identity Int
range = Interval 0 4
