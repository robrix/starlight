{-# LANGUAGE DataKinds, TypeOperators #-}
module UI.Graph.Shader
( points
, pointsVertex
, pointsFragment
, lines
, linesVertex
, linesFragment
) where

import GL.Shader.DSL
import Prelude hiding (lines)

points :: Prog
  '[ "matrix"     '::: M33 Float
   , "pointSize"  '::: Float
   , "colour"     '::: Colour Float ]
  '[ "pos"        '::: V2 Float ]
  '[ "fragColour" '::: Colour Float ]
points = V pointsVertex $ F pointsFragment Nil

pointsVertex :: Shader 'Vertex
  '[ "matrix"    '::: M33 Float
   , "pointSize" '::: Float ]
  '[ "pos"       '::: V2 Float ]
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


lines :: Prog
  '[ "matrix"     '::: M33 Float
   , "colour"     '::: V4 Float ]
  '[ "pos"        '::: V2 Float ]
  '[ "fragColour" '::: Colour Float ]
lines = V linesVertex $ F linesFragment Nil

linesVertex :: Shader 'Vertex
  '[ "matrix" '::: M33 Float ]
  '[ "pos"    '::: V2 Float ]
  '[]
linesVertex = uniforms $ \ matrix -> inputs $ \ pos -> main $ do
  gl_Position .= vec4 (vec3 ((matrix !* vec3 pos 1) ^. _xy) 0) 1

linesFragment :: Shader 'Fragment
  '[ "colour"     '::: Colour Float ]
  '[]
  '[ "fragColour" '::: Colour Float ]
linesFragment = uniforms $ \ colour -> outputs $ \ fragColour -> main $ do
  fragColour .= colour
