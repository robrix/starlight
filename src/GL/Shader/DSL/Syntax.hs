{-# LANGUAGE QuantifiedConstraints #-}
module GL.Shader.DSL.Syntax
( Expr(..)
) where

import Data.Coerce

class (forall a b . Coercible a b => Coercible (expr a) (expr b)) => Expr expr where
  (!*!) :: expr (u (v a)) -> expr (v (w a)) -> expr (u (w a))
