{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
module Starlight.Starfield
( drawStarfield
, runStarfield
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
import           Linear.Affine
import           Linear.V2
import           Linear.Vector
import           Starlight.Starfield.Shader
import           Starlight.View
import qualified UI.Drawable as UI

drawStarfield
  :: ( Has Check sig m
     , Has (Lift IO) sig m
     , Has Profile sig m
     , Has (Reader Drawable) sig m
     , Has (Reader View) sig m
     )
  => m ()
drawStarfield = measure "starfield" . UI.using getDrawable $ do
  View{ scale, size, zoom, focus } <- ask

  resolution_ ?= (fromIntegral <$> size ^* scale)
  origin_     ?= focus / P (fromIntegral <$> size)
  zoom_       ?= zoom

  drawArrays TriangleStrip range


runStarfield
  :: ( Has Check sig m
     , Has Finally sig m
     , Has (Lift IO) sig m
     )
  => ReaderC Drawable m a
  -> m a
runStarfield m = do
  program <- build shader
  array   <- load vertices
  runReader (Drawable UI.Drawable{ program, array }) m


newtype Drawable = Drawable { getDrawable :: UI.Drawable U V O }


vertices :: [V Identity]
vertices = coerce @[V2 Float]
  [ V2 (-1) (-1)
  , V2   1  (-1)
  , V2 (-1)   1
  , V2   1    1
  ]

range :: Interval Identity Int
range = Interval 0 (Identity (length vertices))
