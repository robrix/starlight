{-# LANGUAGE DataKinds, TypeOperators #-}
module Starlight.Shader.Stars
( vertex
) where

import GL.Shader.DSL

vertex :: Shader 'Vertex
  '[]
  '[ "position2" '::: V2 Float ]
  '[]
vertex = inputs $ \ position2 -> main $
  gl_Position .= vec4 (vec3 position2 0) 1
