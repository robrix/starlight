{-# LANGUAGE QuantifiedConstraints #-}
module GL.Shader.DSL.Syntax
( Expr(..)
) where

import Data.Coerce

class (forall a b . Coercible a b => Coercible (expr a) (expr b)) => Expr expr where
