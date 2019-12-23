{-# LANGUAGE DataKinds, KindSignatures #-}
module GL.Shader.DSL
( Shader
, Expr
) where

import Data.DSL

data Shader (u :: Context) (i :: Context) (o :: Context)
data Expr a
