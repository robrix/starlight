{-# LANGUAGE DataKinds, KindSignatures #-}
module GL.Shader.DSL
( Shader
, Expr
) where

import Data.DSL
import GL.Shader (Type(..))

data Shader (t :: Type) (u :: Context) (i :: Context) (o :: Context)
data Expr a
