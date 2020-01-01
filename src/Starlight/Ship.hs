{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
module Starlight.Ship
( makeDrawShip
, DrawShip
, drawShip
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
import Linear.Affine
import Linear.Exts
import Linear.Matrix
import Linear.V2
import Linear.V4
import Linear.Vector
import Starlight.Actor
import Starlight.Ship.Shader as Ship
import Starlight.View
import UI.Colour

makeDrawShip
  :: ( Has Finally sig m
     , Has (Lift IO) sig m
     )
  => m DrawShip
makeDrawShip = do
  program <- build Ship.shader
  array <- load vertices
  pure DrawShip
    { drawShip = \ colour Actor{ position, rotation } -> measure "ship" . use program . bindArray array $ do
      vs@ViewScale{ focus } <- ask
      let matrix = scaleToViewZoomed vs
      set Ship.U
        { matrix = Just
            $   matrix
            !*! translated3 (ext (negated (unP focus)) 0)
            !*! translated3 (ext (unP position) 0)
            !*! scaled (V4 15 15 15 1)
            !*! mkTransformation rotation 0
        , colour = Just colour
        }
      drawArrays LineLoop range
    }

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


vertices :: [Ship.V I]
vertices = coerce @[V2 Float]
  [ V2 1      0
  , V2 0      (-0.5)
  , V2 (-0.5) 0
  , V2 0      0.5
  ]

range :: Interval I Int
range = Interval 0 4
