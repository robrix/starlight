{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}
module Starlight.Shader.Stars
( shader
, U(..)
) where

import GHC.Generics (Generic)
import GL.Shader.DSL

-- based on Star Nest by Pablo Roman Andrioli: https://www.shadertoy.com/view/XlfGRj

shader :: Shader U I O
shader = V vertex $ F fragment Nil where
  vertex U {} I { pos } None =
    gl_Position .= vec4 (vec3 pos 0) 1

  fragment U { resolution, origin, zoom } None O { fragColour } = do
      uv <- let' "uv" $ (gl_FragCoord ^. _xy / resolution ^. _xy - 0.5) * vec2 1 (resolution ^. _y / resolution ^. _x)
      dir <- let' "dir" $ vec3 (uv ^* zoom) 1 ^* 0.5
      origin <- let' "origin" $ vec3 (coerce origin ^* 0.05) 1
      s <- var "s" 0.1
      fade <- var "fade" 0.5
      v <- var "v" $ vec3 (vec2 0 0) 0
      r <- var @Int "r" 0
      while (get r `lt` volsteps) $ do
        p <- var "p" $ origin + dir ^* get s
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
        a *= get a * get a
        iff (get r `gt` 6)
          (fade *= 1.0 - dm)
          (pure ())
        v += vec3 (vec2 (get fade) (get fade)) (get fade)
        v += vec3 (vec2 (get s) (get s * get s)) (get s * get s * get s) ^* get a ^* brightness ^* get fade
        fade *= distfading
        s += stepsize
        r += 1
      mag <- let' "mag" (norm (get v))
      v .= lerp saturation (vec3 (vec2 mag mag) mag) (get v)
      fragColour .= vec4 (get v ^* 0.01) 1
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
  deriving (Generic, Vars)

newtype I v = I { pos :: v (V2 Float) }
  deriving (Generic, Vars)

newtype O v = O { fragColour :: v (Colour Float) }
  deriving (Generic, Vars)
