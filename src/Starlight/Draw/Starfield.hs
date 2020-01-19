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
import qualified Data.Fixed as Fixed
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
  let V2 (q1, r1) (q2, r2) = (`Fixed.divMod'` 1) . convert <$> focus
  focusR_     ?= (fmap realToFrac <$> V2 r1 r2)
  focusQ_     ?= (realToFrac <$> V2 q1 (q2 :: Integer))
  zoom_       ?= realToFrac zoom

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
shader = program $ \ U{ resolution, focusQ, focusR, zoom }
  ->  vertex (\ V{ pos } None -> main $
    gl_Position .= ext4 (ext3 pos 0) 1)

  >>> fragment (\ None Frag{ fragColour } -> main $ do
    resolution <- let' @(V2 Float) "resolution" (D.coerce resolution)
    uv <- let' "uv" $ (gl_FragCoord^._xy / resolution^._xy - 0.5) * vec2 [1, resolution^._y / resolution^._x]
    dir <- var "dir" $ ext3 (uv D.^* zoom) 1 D.^* 0.5
    focusR <- var @(V3 Float) "focusR" $ ext3 focusR 1
    focusQ <- var @(V3 Float) "focusQ" $ ext3 focusQ 1
    let wrap mn mx x = ((x + mx) `mod'` (mx - mn)) + mn
    nf <- let' "nf" (norm (get focusQ + get focusR))
    a1 <- let' "a1" $ wrap (-pi) pi (0.3 + 0.1/nf)
    a2 <- let' "a2" $ wrap (-pi) pi (0.2 + 0.1/nf)
    rot1 <- let' "rot1" $ mat2 [vec2 [cos a1, sin a1], vec2 [-sin a1, cos a1]]
    rot2 <- let' "rot2" $ mat2 [vec2 [cos a2, sin a2], vec2 [-sin a2, cos a2]]
    dir^^._xz *!= rot1
    dir^^._xy *!= rot2
    focusR^^._xz *!= rot1
    focusR^^._xy *!= rot2
    focusQ^^._xz *!= rot1
    focusQ^^._xy *!= rot2
    focusR *= vec3 [10]
    focusQ *= vec3 [10]
    s <- var "s" 0.1
    fade <- var "fade" 0.5
    v <- var "v" $ vec3 [0]
    r <- var @Int "r" 0
    while (get r `lt` volsteps) $ do
      p <- var "p" $ get focusR + get focusQ + get dir D.^* get s
      p .= abs (vec3 [tile] - mod' (get p) (vec3 [tile * 2]))
      pa <- var "pa" 0
      a <- var "a" 0
      i <- var @Int "i" 0
      while (get i `lt` iterations) $ do
        p .= abs (get p) / dot (get p) (get p) - formuparam
        a += abs (norm (get p) - get pa)
        pa .= norm (get p)
        i += 1
      dm <- let' "dm" $ max' 0 (darkmatter - get a * get a * 0.001)
      a *= get a ** 2
      iff (get r `gt` 6)
        (fade *= 1.0 - dm)
        (pure ())
      v += vec3 [get fade]
      v += vec3 [get s, get s ** 2, get s ** 3] D.^* get a D.^* brightness D.^* get fade
      fade *= distfading
      s += stepsize
      r += 1
    mag <- let' "mag" (norm (get v))
    v .= lerp saturation (vec3 [mag]) (get v)
    fragColour .= ext4 (get v D.^* 0.01) 1)
  where
  iterations = 17
  formuparam = 0.53
  volsteps = 8
  stepsize = 0.1
  tile = 1/1.61803398875
  brightness = 0.0015
  darkmatter = 0.3
  distfading = 0.73
  saturation = 0.85


data U v = U
  { resolution :: v (V2 (Window.Pixels Float))
  , focusQ     :: v (V2 (Giga Metres Float))
  , focusR     :: v (V2 (Giga Metres Float))
  , zoom       :: v Float
  }
  deriving (Generic)

instance Vars U

resolution_ :: Lens' (U v) (v (V2 (Window.Pixels Float)))
resolution_ = field @"resolution"

focusQ_ :: Lens' (U v) (v (V2 (Giga Metres Float)))
focusQ_ = field @"focusQ"

focusR_ :: Lens' (U v) (v (V2 (Giga Metres Float)))
focusR_ = field @"focusR"

zoom_ :: Lens' (U v) (v Float)
zoom_ = field @"zoom"

newtype V v = V { pos :: v (V2 Float) }
  deriving (Generic)

instance Vars V

deriving via Fields V instance Storable (V I)
