{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Starlight.Starfield
( starfield
, drawStarfield
, Starfield
) where

import           Control.Effect.Finally
import           Control.Effect.Lift
import           Control.Effect.Profile
import           Control.Effect.Reader
import           Data.Coerce (coerce)
import           Data.Functor.I
import           Data.Functor.Interval
import           GL.Array
import           GL.Program
import           Linear.Affine
import           Linear.V2
import           Linear.Vector
import qualified Starlight.Starfield.Shader as Starfield
import           Starlight.View

starfield
  :: ( Has Finally sig m
     , Has (Lift IO) sig m
     )
  => m Starfield
starfield = do
  program <- build Starfield.shader
  array <- load vertices
  pure Starfield { program, array }


drawStarfield
  :: ( Has (Lift IO) sig m
     , Has Profile sig m
     , Has (Reader ViewScale) sig m
     )
  => Starfield
  -> Point V2 Float
  -> m ()
drawStarfield Starfield{ program, array } position = measure "starfield" . use program . bindArray array $ do
  ViewScale{ scale, size, zoom } <- ask

  set Starfield.U
    { resolution = Just (fromIntegral <$> size ^* scale)
    , origin     = Just (position / P (fromIntegral <$> size))
    , zoom       = Just zoom
    }

  drawArrays TriangleStrip range


data Starfield = Starfield
  { program :: Program Starfield.U Starfield.V Starfield.O
  , array   :: Array (Starfield.V I)
  }


vertices :: [Starfield.V I]
vertices = coerce @[V2 Float]
  [ V2 (-1) (-1)
  , V2   1  (-1)
  , V2 (-1)   1
  , V2   1    1
  ]

range :: Interval I Int
range = Interval 0 (I (length vertices))
