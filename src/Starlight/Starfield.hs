{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
module Starlight.Starfield
( makeDrawStarfield
, DrawStarfield
, drawStarfield
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

makeDrawStarfield
  :: ( Has Finally sig m
     , Has (Lift IO) sig m
     )
  => m DrawStarfield
makeDrawStarfield = do
  program <- build Starfield.shader
  array <- load vertices
  pure DrawStarfield
    { drawStarfield = measure "starfield" . use program . bindArray array $ do
      View{ scale, size, zoom, focus } <- ask

      set Starfield.U
        { resolution = Just (fromIntegral <$> size ^* scale)
        , origin     = Just (focus / P (fromIntegral <$> size))
        , zoom       = Just zoom
        }

      drawArrays TriangleStrip range
    }


newtype DrawStarfield = DrawStarfield
  { drawStarfield
    :: forall sig m
    .  ( Has (Lift IO) sig m
       , Has Profile sig m
       , Has (Reader View) sig m
       )
    => m ()
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
