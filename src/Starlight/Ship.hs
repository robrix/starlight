{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
module Starlight.Ship
( ship
, runShip
, DrawShip
) where

import Control.Effect.Finally
import Control.Effect.Lift
import Control.Effect.Profile
import Control.Carrier.Reader
import Data.Coerce (coerce)
import Data.Functor.I
import Data.Functor.Interval
import GL.Array
import GL.Program
import Linear.Affine
import Linear.Exts
import Linear.Matrix
import Linear.V2
import Linear.V4
import Linear.Vector
import Starlight.Actor
import Starlight.Ship.Shader
import Starlight.View
import UI.Colour

ship
  :: ( Has (Lift IO) sig m
     , Has Profile sig m
     , Has (Reader DrawShip) sig m
     , Has (Reader View) sig m
     )
  => Colour Float
  -> Actor
  -> m ()
ship colour Actor{ position, rotation } = do
  DrawShip{ program, array } <- ask
  measure "ship" . use program . bindArray array $ do
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
  => ReaderC DrawShip m a
  -> m a
runShip m = do
  program <- build shader
  array   <- load vertices
  runReader DrawShip{ program, array } m


data DrawShip = DrawShip
  { program :: Program U V O
  , array   :: Array (V I)
  }


vertices :: [V I]
vertices = coerce @[V2 Float]
  [ V2 1      0
  , V2 0      (-0.5)
  , V2 (-0.5) 0
  , V2 0      0.5
  ]

range :: Interval I Int
range = Interval 0 4
