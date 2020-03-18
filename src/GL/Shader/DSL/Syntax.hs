{-# LANGUAGE QuantifiedConstraints #-}
module GL.Shader.DSL.Syntax
( Expr(..)
, false
, true
) where

import Data.Coerce
import Linear.V2
import Linear.V3
import Linear.V4

class ( forall a b . Coercible a b => Coercible (expr a) (expr b)
      , forall a . Num a => Num (expr a)
      )
   => Expr expr where
  -- matrix

  (!*!) :: expr (u (v a)) -> expr (v (w a)) -> expr (u (w a))
  (!*) :: expr (u (v a)) -> expr (v a) -> expr (u a)

  infixl 7 !*!, !*

  -- vector

  ext3 :: expr (V2 a) -> expr a -> expr (V3 a)
  ext4 :: expr (V3 a) -> expr a -> expr (V4 a)

  -- general syntax

  let' :: expr a -> (expr a -> expr b) -> expr b

  -- booleans

  bool :: Bool -> expr Bool
  iff :: expr Bool -> expr a -> expr a -> expr a

false, true :: Expr expr => expr Bool
false = bool False
true  = bool True
