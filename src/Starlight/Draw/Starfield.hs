{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
module Starlight.Draw.Starfield
( drawStarfield
, runStarfield
, Drawable
) where

import           Control.Carrier.Reader
import           Control.Effect.Finally
import           Control.Effect.Lens ((?=))
import           Control.Effect.Lift
import           Control.Effect.Trace
import           Control.Lens (Lens')
import           Data.Coerce (coerce)
import           Data.Functor.Identity
import           Data.Functor.Interval hiding (max')
import           Data.Generics.Product.Fields
import           Foreign.Storable (Storable)
import           GHC.Generics (Generic)
import           GL.Array
import           GL.Effect.Check
import           GL.Object
import           GL.Shader.DSL hiding (coerce, (!*!), (^*))
import qualified GL.Shader.DSL as D
import           Linear.V2 hiding (R1(..), R2(..))
import           Starlight.View
import qualified UI.Drawable as UI
import qualified UI.Window as Window
import           Unit.Length

drawStarfield
  :: ( Has Check sig m
     , Has (Lift IO) sig m
     , Has (Reader Drawable) sig m
     , Has (Reader View) sig m
     )
  => m ()
drawStarfield = UI.using getDrawable $ do
  View{ size, zoom, focus } <- ask

  resolution_ ?= (fromIntegral <$> size)
  focus_      ?= (fmap realToFrac . convert <$> focus)
  zoom_       ?= realToFrac zoom

  drawArrays TriangleStrip range


runStarfield
  :: ( Has Check sig m
     , Has Finally sig m
     , Has (Lift IO) sig m
     , Has Trace sig m
     , Effect sig
     )
  => ReaderC Drawable m a
  -> m a
runStarfield = UI.loadingDrawable Drawable shader vertices


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


-- based on Star Nest by Pablo Roman Andrioli: https://www.shadertoy.com/view/XlfGRj

shader :: Shader U V O
shader = program $ \ U{ resolution, focus, zoom }
  ->  vertex (\ V{ pos } None -> main $
    gl_Position .= ext4 (ext3 pos 0) 1)

  >>> fragment (\ None O{ fragColour } -> main $ do
    resolution <- let' "resolution" (D.coerce resolution)
    uv <- let' "uv" $ (gl_FragCoord^._xy / resolution^._xy - 0.5) * vec2 [1, resolution^._y / resolution^._x]
    dir <- var "dir" $ ext3 (uv D.^* zoom) 1 D.^* 0.5
    focus <- var "focus" $ ext3 (D.coerce focus / (resolution D.^* 0.1)) 1
    a1 <- let' "a1" $ sqrt (0.2 + norm (get focus) / 1000 / resolution^._x * 2)
    a2 <- let' "a2" $ sqrt (0.2 + norm (get focus) / 1000 / resolution^._y * 2)
    rot1 <- let' "rot1" $ mat2 (vec2 [cos a1, sin a1]) (vec2 [-sin a1, cos a1])
    rot2 <- let' "rot2" $ mat2 (vec2 [cos a2, sin a2]) (vec2 [-sin a2, cos a2])
    dir^^._xz *!= rot1
    dir^^._xy *!= rot2
    focus^^._xz *!= rot1
    focus^^._xy *!= rot2
    s <- var "s" 0.1
    fade <- var "fade" 0.5
    v <- var "v" $ vec3 [0]
    r <- var @Int "r" 0
    while (get r `lt` volsteps) $ do
      p <- var "p" $ get focus + get dir D.^* get s
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
  tile = 0.85
  brightness = 0.0015
  darkmatter = 0.3
  distfading = 0.73
  saturation = 0.85


data U v = U
  { resolution :: v (V2 (Window.Pixels Float))
  , focus      :: v (V2 (Giga Metres Float))
  , zoom       :: v Float
  }
  deriving (Generic)

instance Vars U

resolution_ :: Lens' (U v) (v (V2 (Window.Pixels Float)))
resolution_ = field @"resolution"

focus_ :: Lens' (U v) (v (V2 (Giga Metres Float)))
focus_ = field @"focus"

zoom_ :: Lens' (U v) (v Float)
zoom_ = field @"zoom"

newtype V v = V { pos :: v (V2 Float) }
  deriving (Generic)

instance Vars V

deriving instance Bind     (v (V2 Float)) => Bind     (V v)
deriving instance Storable (v (V2 Float)) => Storable (V v)

newtype O v = O { fragColour :: v (Colour Float) }
  deriving (Generic)

instance Vars O
