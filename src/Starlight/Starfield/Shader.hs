{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
module Starlight.Starfield.Shader
( shader
, U(..)
, resolution_
, origin_
, zoom_
, V(..)
, O(..)
) where

import Control.Lens (Lens')
import Data.Generics.Product.Fields
import Foreign.Storable (Storable)
import GHC.Generics (Generic)
import GL.Object
import GL.Shader.DSL

-- based on Star Nest by Pablo Roman Andrioli: https://www.shadertoy.com/view/XlfGRj

shader :: Shader U V O
shader = program $ \ U{ resolution, origin, zoom }
  ->  vertex (\ V{ pos } None ->
    gl_Position .= ext4 (ext3 pos 0) 1)
  >>> fragment (\ None O{ fragColour } -> do
    uv <- let' "uv" $ (gl_FragCoord ^. _xy / resolution ^. _xy - 0.5) * vec2 1 (resolution ^. _y / resolution ^. _x)
    dir <- var "dir" $ ext3 (uv ^* zoom) 1 ^* 0.5
    origin <- var "origin" $ ext3 (coerce origin / (resolution ^* 0.1)) 1
    a1 <- let' "a1" $ 0.2 + norm (get origin) / resolution ^. _x * 2
    a2 <- let' "a2" $ 0.2 + norm (get origin) / resolution ^. _y * 2
    rot1 <- let' "rot1" $ mat2 (vec2 (cos a1) (sin a1)) (vec2 (-(sin a1)) (cos a1))
    rot2 <- let' "rot2" $ mat2 (vec2 (cos a2) (sin a2)) (vec2 (-(sin a2)) (cos a2))
    dir ^^. _xz *!= rot1
    dir ^^. _xy *!= rot2
    origin ^^. _xz *!= rot1
    origin ^^. _xy *!= rot2
    s <- var "s" 0.1
    fade <- var "fade" 0.5
    v <- var "v" $ vec3 0 0 0
    r <- var @Int "r" 0
    while (get r `lt` volsteps) $ do
      p <- var "p" $ get origin + get dir ^* get s
      p .= abs (tile - mod' (get p) (tile ^* 2))
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
      v += vec3 (get fade) (get fade) (get fade)
      v += vec3 (get s) (get s ** 2) (get s ** 3) ^* get a ^* brightness ^* get fade
      fade *= distfading
      s += stepsize
      r += 1
    mag <- let' "mag" (norm (get v))
    v .= lerp saturation (vec3 mag mag mag) (get v)
    fragColour .= ext4 (get v ^* 0.01) 1)
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
  { resolution :: v (V2 Float)
  , origin     :: v (Point V2 Float)
  , zoom       :: v Float
  }
  deriving (Generic)

instance Vars U

resolution_ :: Lens' (U v) (v (V2 Float))
resolution_ = field @"resolution"

origin_ :: Lens' (U v) (v (Point V2 Float))
origin_ = field @"origin"

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
