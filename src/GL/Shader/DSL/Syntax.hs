{-# LANGUAGE QuantifiedConstraints #-}
module GL.Shader.DSL.Syntax
( Expr(..)
) where

import Data.Coerce
import Linear.V2
import Linear.V3
import Linear.V4

class ( forall a b . Coercible a b => Coercible (expr a) (expr b)
      , forall a . Num a => Num (expr a)
      )
   => Expr expr where
  (!*!) :: expr (u (v a)) -> expr (v (w a)) -> expr (u (w a))

  (!*) :: expr (u (v a)) -> expr (v a) -> expr (u a)

  infixl 7 !*!, !*

  ext3 :: expr (V2 a) -> expr a -> expr (V3 a)
  ext4 :: expr (V3 a) -> expr a -> expr (V4 a)

  let' :: expr a -> (expr a -> expr b) -> expr b

  bool :: Bool -> expr Bool
  iff :: expr Bool -> expr a -> expr a -> expr a
