{-# LANGUAGE DataKinds, KindSignatures #-}
module GL.Shader.DSL
( Shader
, Expr
, main
) where

import Data.DSL
import GL.Shader (Type(..))

data Shader (t :: Type) (u :: Context) (i :: Context) (o :: Context)
data Expr a

main :: Expr () -> Shader k u i o
main _ = undefined
