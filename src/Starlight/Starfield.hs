{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
module Starlight.Starfield
( starfield
, runStarfield
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
import           Linear.V2
import           Linear.Vector
import           Starlight.Starfield.Shader
import           Starlight.View
import qualified UI.Drawable as UI

starfield
  :: ( Has (Lift IO) sig m
     , Has Profile sig m
     , Has (Reader Drawable) sig m
     , Has (Reader View) sig m
     )
  => m ()
starfield = do
  UI.Drawable { program, array } <- asks getDrawable
  measure "starfield" . use program . bindArray array $ do
    View{ scale, size, zoom, focus } <- ask

    set U
      { resolution = Just (fromIntegral <$> size ^* scale)
      , origin     = Just (focus / P (fromIntegral <$> size))
      , zoom       = Just zoom
      }

    drawArrays TriangleStrip range


runStarfield
  :: ( Has Finally sig m
     , Has (Lift IO) sig m
     )
  => ReaderC Drawable m a
  -> m a
runStarfield m = do
  program <- build shader
  array   <- load vertices
  runReader (Drawable UI.Drawable{ program, array }) m


newtype Drawable = Drawable { getDrawable :: UI.Drawable U V O }


vertices :: [V I]
vertices = coerce @[V2 Float]
  [ V2 (-1) (-1)
  , V2   1  (-1)
  , V2 (-1)   1
  , V2   1    1
  ]

range :: Interval I Int
range = Interval 0 (I (length vertices))
