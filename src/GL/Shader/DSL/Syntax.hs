{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE QuantifiedConstraints #-}
module GL.Shader.DSL.Syntax
( Expr(..)
, false
, true
) where

import Data.Coerce
import Data.Kind (Type)
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

  infixr 1 >>>

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

  -- general syntax

  let' :: expr s a -> (expr s a -> expr s b) -> expr s b

  case' :: expr s a -> [(a, expr s b)] -> expr s b

  -- booleans

  bool :: Bool -> expr s Bool
  iff :: expr s Bool -> expr s a -> expr s a -> expr s a

false, true :: Expr expr => expr s Bool
false = bool False
true  = bool True
