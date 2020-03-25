{-# LANGUAGE RankNTypes #-}
module GL.Shader.GLSL.Syntax
( Ref
, Stmt(..)
) where

import GL.Shader.GLSL.Type

newtype Ref a = Ref { getRef :: String }

class Stmt stmt where
  var :: (forall ty . (Type ty, FType ty, VType ty, MType ty) => ty a) -> stmt (Ref a)
