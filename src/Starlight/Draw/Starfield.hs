{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
module Starlight.Draw.Starfield
( draw
, Starlight.Draw.Starfield.run
, Drawable
) where

import           Control.Carrier.Reader
import           Control.Effect.Finally
import           Control.Effect.Lens ((?=))
import           Control.Effect.Lift
import           Control.Effect.Trace
import           Control.Lens (Lens')
import           Data.Coerce (coerce)
import           Data.Functor.I
import           Data.Functor.Interval hiding (range, max')
import           Data.Generics.Product.Fields
import           Foreign.Storable (Storable)
import           GHC.Generics (Generic)
import           GL.Array
import           GL.Effect.Check
import           GL.Shader.DSL hiding (coerce, (!*!), (^*))
import qualified GL.Shader.DSL as D
import           Linear.V2 hiding (R1(..), R2(..))
import           Starlight.View
import qualified UI.Drawable as UI
import qualified UI.Window as Window
import           Unit.Length

draw
  :: ( Has Check sig m
     , Has (Lift IO) sig m
     , Has (Reader Drawable) sig m
     , Has (Reader View) sig m
     )
  => m ()
draw = UI.using getDrawable $ do
  view@View{ zoom, focus } <- ask

  resolution_ ?= (fromIntegral <$> contextSize view)
  focus_      ?= focus
  zoom_       ?= realToFrac (1/zoom)

  drawArrays TriangleStrip range


run
  :: ( Has Check sig m
     , Has Finally sig m
     , Has (Lift IO) sig m
     , Has Trace sig m
     , Effect sig
     )
  => ReaderC Drawable m a
  -> m a
run = UI.loadingDrawable Drawable shader vertices


newtype Drawable = Drawable { getDrawable :: UI.Drawable U V Frag }


vertices :: [V I]
vertices = coerce @[V2 Float]
  [ V2 (-1) (-1)
  , V2   1  (-1)
  , V2 (-1)   1
  , V2   1    1
  ]

range :: Interval I Int
range = Interval 0 (I (length vertices))


-- based on Star Nest by Pablo Roman Andrioli: https://www.shadertoy.com/view/XlfGRj

shader :: Shader U V Frag
shader = program $ \ U{ resolution, focus, zoom }
  ->  vertex (\ V{ pos } None -> main $
    gl_Position .= ext4 (ext3 pos 0) 1)

  >>> fragment (\ None Frag{ fragColour } -> main $ do
    resolution <- let' @(V2 Float) "resolution" (D.coerce resolution)
    uv <- let' "uv" $ (gl_FragCoord^._xy / resolution^._xy - 0.5) * vec2 [1, resolution^._y / resolution^._x]
    dir <- var "dir" $ ext3 (uv D.^* zoom) 1 D.^* 0.5
    focus <- let' @(V3 Double) "focus" $ dext3 focus 1
    let wrap x = ((x + pi) `mod'` (pi * 2)) - pi
    nf <- let' "nf" (float (0.1 / norm focus))
    focus <- var "focus2" $ vec3 [ focus `mod'` dvec3 [tile * 2] ]
    a1 <- let' "a1" (wrap (0.3 + nf))
    cos_a1 <- let' "cos_a1" (cos a1)
    sin_a1 <- let' "sin_a1" (sin a1)
    rot1 <- let' "rot1" $ mat2 [vec2 [cos_a1, sin_a1], vec2 [-sin_a1, cos_a1]]
    a2 <- let' "a2" (wrap (0.2 + nf))
    cos_a2 <- let' "cos_a2" (cos a2)
    sin_a2 <- let' "sin_a2" (sin a2)
    rot2 <- let' "rot2" $ mat2 [vec2 [cos_a2, sin_a2], vec2 [-sin_a2, cos_a2]]
    dir^^._xz *!= rot1
    dir^^._xy *!= rot2
    focus^^._xz *!= rot1
    focus^^._xy *!= rot2
    focus *= 10
    fade <- var "fade" 0.5
    v <- var "v" $ vec3 [0]
    r <- var @Int "r" 0
    while (get r `lt` volsteps) $ do
      s <- let' "s" (0.1 * float (get r + 1))
      p <- var "p" $ get focus + get dir D.^* s
      p .= abs (vec3 [tile] - (get p `mod'` vec3 [tile * 2]))
      pa <- var "pa" 0
      a <- var "a" 0
      i <- var @Int "i" 0
      while (get i `lt` iterations) $ do
        p .= abs (get p) / dot (get p) (get p) - formuparam
        prev <- let' "prev" (get pa)
        pa .= norm (get p)
        a += abs (get pa - prev)
        i += 1
      a .= get a ** 3
      v += vec3 [s, s ** 2, s ** 3] D.^* get a D.^* brightness D.^* get fade
      fade *= distfading
      r += 1
    mag <- let' "mag" (norm (get v))
    v .= lerp saturation (vec3 [mag]) (get v)
    fragColour .= ext4 (get v D.^* 0.01) 1)
  where
  iterations = 17
  formuparam = 0.53
  volsteps = 8
  tile = 1/1.61803398875
  brightness = 0.0015
  distfading = 0.73
  saturation = 0.65


data U v = U
  { resolution :: v (V2 (Window.Pixels Float))
  , focus      :: v (V2 (Giga Metres Double))
  , zoom       :: v Float
  }
  deriving (Generic)

instance Vars U

resolution_ :: Lens' (U v) (v (V2 (Window.Pixels Float)))
resolution_ = field @"resolution"

focus_ :: Lens' (U v) (v (V2 (Giga Metres Double)))
focus_ = field @"focus"

zoom_ :: Lens' (U v) (v Float)
zoom_ = field @"zoom"

newtype V v = V { pos :: v (V2 Float) }
  deriving (Generic)

instance Vars V

deriving via Fields V instance Storable (V I)
