{-# LANGUAGE DataKinds, TypeOperators #-}
module UI.Graph.Lines
( shader
) where

import GL.Shader.DSL

shader :: Prog
  '[ "matrix"     '::: M33 Float
   , "colour"     '::: V4 Float ]
  '[ "pos"        '::: V2 Float ]
  '[ "fragColour" '::: Colour Float ]
shader = V vertex $ F fragment Nil

vertex :: Shader 'Vertex
  '[ "matrix" '::: M33 Float ]
  '[ "pos"    '::: V2 Float ]
  '[]
vertex = uniforms $ \ matrix -> inputs $ \ pos -> main $ do
  gl_Position .= vec4 (vec3 ((matrix !* vec3 pos 1) ^. _xy) 0) 1

fragment :: Shader 'Fragment
  '[ "colour"     '::: Colour Float ]
  '[]
  '[ "fragColour" '::: Colour Float ]
fragment = uniforms $ \ colour -> outputs $ \ fragColour -> main $ do
  fragColour .= colour
