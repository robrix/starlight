module GL.Shader.GLSL.Syntax
( Type(..)
, VType(..)
, MType(..)
, N(..)
, Ref
, Stmt(..)
) where

data Type
  = Bool
  | Int
  | UInt
  | Float
  | Double
  | Vec VType N
  | Mat MType N

data VType
  = VB
  | VI
  | VU
  | VF
  | VD

data MType
  = MF
  | MD

data N
  = N1
  | N2
  | N3
  | N4

newtype Ref a = Ref { getRef :: String }

class Stmt stmt where
  var :: Type -> stmt (Ref a)
