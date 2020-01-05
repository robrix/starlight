{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Starlight.Draw.Body
( runBody
, Drawable
, drawBody
) where

import           Control.Carrier.Reader
import           Control.Effect.Finally
import           Control.Effect.Lens ((?=))
import           Control.Effect.Lift
import           Control.Effect.Profile
import           Data.Coerce
import           Data.Functor.Identity
import           Data.Functor.Interval
import           Geometry.Circle
import           GL.Array
import           GL.Effect.Check
import           GL.Program
import           Linear.Exts
import           Starlight.Actor
import           Starlight.Body
import           Starlight.Draw.Body.Shader as Shader
import           Starlight.View
import qualified UI.Drawable as UI
import           Unit.Length

runBody
  :: ( Has Check sig m
     , Has Finally sig m
     , Has (Lift IO) sig m
     , Effect sig
     )
  => ReaderC Drawable m a
  -> m a
runBody m = do
  program <- build Shader.shader
  array   <- load vertices
  runReader (Drawable UI.Drawable{ program, array }) m

drawBody
  :: ( Has Check sig m
     , Has (Lift IO) sig m
     , Has Profile sig m
     , Has (Reader Drawable) sig m
     , Has (Reader View) sig m
     )
  => StateVectors
  -> m ()
drawBody StateVectors{ body = Body{ radius = Metres r, colour }, transform, actor = Actor{ rotation } } = measure "bodies" . UI.using getDrawable $ do
  vs@View{ focus } <- ask
  matrix_ ?=
        scaleToViewZoomed vs
    !*! translated3 (ext (negated (unP focus)) 0) -- transform to the origin
    !*! transform
    !*! scaled (V4 r r r 1)
    !*! mkTransformation rotation 0
  colour_ ?= colour

  drawArraysInstanced LineLoop range 3


newtype Drawable = Drawable { getDrawable :: UI.Drawable Shader.U Shader.V Shader.O }


vertices :: [Shader.V Identity]
vertices = coerce @[V4 Float] . map (`ext` V2 0 1) $ circle 1 128

range :: Interval Identity Int
range = Interval 0 (Identity (length vertices))
