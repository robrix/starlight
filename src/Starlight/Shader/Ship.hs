{-# LANGUAGE DataKinds, TypeOperators #-}
module Starlight.Shader.Ship
( program
, vertex
, fragment
) where

import GL.Shader.DSL
import Linear.Matrix (M33)
import Linear.V2 (V2)
import UI.Colour (Colour)

program :: Prog
  '[ "matrix" '::: M33 Float
   , "colour" '::: Colour Float ]
  '[ "position2" '::: V2 Float ]
  '[ "fragColour" '::: Colour Float ]
program
  =    Stage vertex
  :>>> Stage fragment

vertex
  :: Shader
    'Vertex
    '[ "matrix" '::: M33 Float ]
    '[ "position2" '::: V2 Float ]
    '[]
vertex = uniforms $ \ matrix -> inputs $ \ pos -> main $
  gl_Position .= vec4 (matrix !* vec3 pos 1) 1

fragment
  :: Shader
    'Fragment
    '[ "colour"     '::: Colour Float ]
    '[]
    '[ "fragColour" '::: Colour Float ]
fragment = uniforms $ \ colour -> outputs $ \ fragColour -> main $
  fragColour .= colour
