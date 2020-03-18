{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE QuantifiedConstraints #-}
module GL.Shader.DSL.Syntax
( Expr(..)
, Boolean(..)
, false
, true
) where

import Data.Coerce
import Data.Kind (Type)
import Geometry.Transform
import GL.Shader (Stage(..))
import Linear.V2
import Linear.V3
import Linear.V4

class ( forall a b s . Coercible a b => Coercible (expr s a) (expr s b)
      , forall a s . Num a => Num (expr s a)
      , forall a s . Fractional a => Fractional (expr s a)
      , forall a s . Floating a => Floating (expr s a)
      )
   => Expr (expr :: Stage -> Type -> Type) where
  -- matrix

  (>>>) :: expr s (Transform m a u v) -> expr s (Transform m a v w) -> expr s (Transform m a u w)
  (>*) :: expr s (Transform m a u v) -> expr s (m (v a)) -> expr s (m (u a))

  infixr 1 >>>
  infixl 7 >*

  (!*!) :: expr s (u (v a)) -> expr s (v (w a)) -> expr s (u (w a))
  (!*) :: expr s (u (v a)) -> expr s (v a) -> expr s (u a)

  infixl 7 !*!, !*

  -- vector

  ext3 :: expr s (V2 a) -> expr s a -> expr s (V3 a)
  ext4 :: expr s (V3 a) -> expr s a -> expr s (V4 a)

  norm :: expr s (v a) -> expr s a

  (^*) :: expr s (v a) -> expr s a -> expr s (v a)
  (*^) :: expr s a -> expr s (v a) -> expr s (v a)
  (^/) :: expr s (v a) -> expr s a -> expr s (v a)

  infixl 7 ^*, *^, ^/

  -- scalar

  atan2 :: expr s a -> expr s a -> expr s a

  float :: expr s a -> expr s Float

  min, max :: expr s a -> expr s a -> expr s a

  -- general syntax

  let' :: expr s a -> (expr s a -> expr s b) -> expr s b

  case' :: expr s a -> [(a, expr s b)] -> expr s b

class Boolean expr where
  fromBool :: Bool -> expr Bool
  iff :: expr Bool -> expr a -> expr a -> expr a

false, true :: Boolean expr => expr Bool
false = fromBool False
true  = fromBool True
