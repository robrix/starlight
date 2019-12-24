{-# LANGUAGE DataKinds, DeriveAnyClass, DeriveGeneric, NamedFieldPuns #-}
module Starlight.Shader.Stars
( U(..)
) where

import GHC.Generics (Generic)
import GL.Shader.DSL

-- based on Star Nest by Pablo Roman Andrioli: https://www.shadertoy.com/view/XlfGRj

-- shader :: Shader U I O
-- shader = V vertex $ F fragment Nil where
--   vertex U {} I { pos } None =
--     gl_Position .= vec4 (vec3 pos 0) 1

--   fragment U { resolution, origin, zoom } None O { fragColour } = do
--       uv <- let' "uv" $ (gl_FragCoord ^. _xy / resolution ^. _xy - 0.5) * vec2 1 (resolution ^. _y / resolution ^. _x)
--       dir <- let' "dir" $ vec3 (uv ^* zoom) 1 ^* 0.5
--       origin <- let' "origin" $ vec3 (coerce origin ^* 0.05) 1
--       s <- var "s" (0.1 :: Expr 'Fragment Float)
--       fade <- let' "fade" (0.5 :: Expr 'Fragment Float)
--       v <- let' "v" 0
--       mag <- let' "mag" (norm v)
--       r <- var "r" 0
--       while (r `lt` volsteps) $ do
--         p <- var' "p" $ origin + dir ^* s
--         p .= abs (tile - mod p (tile ^* 2))
--         pa <- var "pa" 0
--         a <- var "a" 0
--         i <- var "i" 0
--         while (i `lt` iterations) $ do
--           p .= abs p / dot p p - formuparam
--           a += abs (norm p - pa)
--           pa .= norm p
--           i += 1
--         dm <- let' "dm" $ max' 0 (darkmatter - a * a * 0.001)
--         a *= a * a
--         iff (r `gt` 6)
--           (fade *= 1.0 - dm)
--           (pure ())
--         v += fade
--         v += vec3 (vec2 s (s * s)) (s * s * s) * a * brightness * fade
--         fade *= distfading
--         s += stepsize
--         r += 1
--       fragColour .= vec4 (lerp saturation (vec3 (vec2 mag mag) mag) v ^* 0.01) 1
--       where
--       iterations = 17
--       formuparam = 0.53
--       volsteps = 8
--       stepsize = 0.1
--       tile = 0.85
--       brightness = 0.0015
--       darkmatter = 0.3
--       distfading = 0.73
--       saturation = 0.85


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
