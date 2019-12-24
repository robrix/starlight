{-# LANGUAGE DataKinds, TypeOperators #-}
module UI.Graph.Shader
( points
, pointsVertex
, pointsFragment
) where

import GL.Shader.DSL

points :: Prog
  '[ "matrix"    '::: M33 Float
   , "pointSize" '::: Float
   , "colour"    '::: Colour Float
   ]
  '[ "pos" '::: V2 Float ]
  '[]
points = V pointsVertex $ F pointsFragment Nil

pointsVertex :: Shader 'Vertex
  '[ "matrix" '::: M33 Float
   , "pointSize" '::: Float
   ]
  '[ "pos" '::: V2 Float ]
  '[]
pointsVertex = uniforms $ \ matrix pointSize -> inputs $ \ pos -> main $ do
  gl_Position .= vec4 (vec3 ((matrix !* vec3 pos 1) ^. _xy) 0) 1
  gl_PointSize .= pointSize

pointsFragment :: Shader 'Fragment
  '[ "colour"     '::: Colour Float ]
  '[]
  '[ "fragColour" '::: Colour Float ]
pointsFragment = uniforms $ \ colour -> outputs $ \ fragColour -> main $ do
  p <- let' "p" (gl_PointCoord - vec2 0.5 0.5)
  iff (norm p `gt` 1)
    discard
    (do
      mag <- let' "mag" (norm p * 2)
      fragColour .= vec4 (colour ^. _xyz) (1 - mag * mag * mag / 2))
