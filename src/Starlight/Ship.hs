{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
module Starlight.Ship
( ship
, runShip
, Drawable
) where

import           Control.Carrier.Reader
import           Control.Effect.Finally
import           Control.Effect.Lift
import           Control.Effect.Profile
import           Data.Coerce (coerce)
import           Data.Functor.I
import           Data.Functor.Interval
import           GL.Array
import           GL.Program
import           Linear.Affine
import           Linear.Exts
import           Linear.Matrix
import           Linear.V2
import           Linear.V4
import           Linear.Vector
import           Starlight.Actor
import           Starlight.Ship.Shader
import           Starlight.View
import           UI.Colour
import qualified UI.Drawable as UI

ship
  :: ( Has (Lift IO) sig m
     , Has Profile sig m
     , Has (Reader Drawable) sig m
     , Has (Reader View) sig m
     )
  => Colour Float
  -> Actor
  -> m ()
ship colour Actor{ position, rotation } = measure "ship" . UI.using getDrawable $ do
  vs@View{ focus } <- ask
  let matrix = scaleToViewZoomed vs
  set U
    { matrix = Just
        $   matrix
        !*! translated3 (ext (negated (unP focus)) 0)
        !*! translated3 (ext (unP position) 0)
        !*! scaled (V4 15 15 15 1)
        !*! mkTransformation rotation 0
    , colour = Just colour
    }
  drawArrays LineLoop range


runShip
  :: ( Has Finally sig m
     , Has (Lift IO) sig m
     )
  => ReaderC Drawable m a
  -> m a
runShip m = do
  program <- build shader
  array   <- load vertices
  runReader (Drawable UI.Drawable{ program, array }) m


newtype Drawable = Drawable { getDrawable :: UI.Drawable U V O }


vertices :: [V I]
vertices = coerce @[V2 Float]
  [ V2 1      0
  , V2 0      (-0.5)
  , V2 (-0.5) 0
  , V2 0      0.5
  ]

range :: Interval I Int
range = Interval 0 4
