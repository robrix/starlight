module GL.Shader.GLSL.Type
( Type(..)
, VType(..)
, MType(..)
) where

import Data.Int
import Data.Word
import Linear.Matrix
import Linear.V2
import Linear.V3
import Linear.V4

class Type ty where
  bool :: ty Bool
  int :: ty Int32
  uint :: ty Word32
  float :: ty Float
  double :: ty Double

class VType ty where
  vec2 :: ty a -> ty (V2 a)
  vec3 :: ty a -> ty (V3 a)
  vec4 :: ty a -> ty (V4 a)

class MType ty where
  mat2 :: ty a -> ty (M22 a)
  mat3 :: ty a -> ty (M33 a)
  mat4 :: ty a -> ty (M44 a)
