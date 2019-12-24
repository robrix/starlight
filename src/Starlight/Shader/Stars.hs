{-# LANGUAGE DataKinds, TypeOperators #-}
module Starlight.Shader.Stars
( vertex
, fragment
) where

import GL.Shader.DSL

-- based on Star Nest by Pablo Roman Andrioli: https://www.shadertoy.com/view/XlfGRj

vertex :: Shader 'Vertex
  '[]
  '[ "position2" '::: V2 Float ]
  '[]
vertex = inputs $ \ position2 -> main $
  gl_Position .= vec4 (vec3 position2 0) 1

fragment :: Shader 'Fragment
  '[ "resolution" '::: V2 Float
   , "origin"     '::: Point V2 Float
   , "zoom"       '::: Float ]
  '[]
  '[ "fragColour" '::: Colour Float ]
fragment = uniforms $ \ resolution origin zoom -> outputs $ \ fragColour -> main $ do
  uv <- let' "uv" $ (gl_FragCoord ^. _xy / resolution ^. _xy - 0.5) * vec2 1 (resolution ^. _y / resolution ^. _x)
  dir <- let' "dir" $ vec3 (uv ^* zoom) 1 ^* 0.5
  origin <- let' "origin" $ vec3 (coerce origin ^* 0.05) 1
  s <- let' "s" (0.1 :: Expr 'Fragment Float)
  fade <- let' "fade" (0.5 :: Expr 'Fragment Float)
  v <- let' "v" 0
  mag <- let' "mag" (norm v)
  fragColour .= vec4 (lerp saturation (vec3 (vec2 mag mag) mag) v ^* 0.01) 1
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
