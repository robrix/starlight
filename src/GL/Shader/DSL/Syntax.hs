{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
module GL.Shader.DSL.Syntax
( Mat(..)
, Vec(..)
, Scalar(..)
, Dim(..)
, Expr(..)
, Boolean(..)
, false
, true
, Geom(..)
) where

import Data.Functor.C
import Data.Functor.I
import Geometry.Transform (Transform)
import Linear.Matrix
import Linear.V2
import Linear.V3
import Linear.V4
import Unit.Algebra (Div, Mul)
import Unit (Unit)
import Unit.Length (Length)

class Mat expr where
  m2 :: expr (V2 a) -> expr (V2 a) -> expr (M22 a)
  m3 :: expr (V3 a) -> expr (V3 a) -> expr (V3 a) -> expr (M33 a)
  m4 :: expr (V4 a) -> expr (V4 a) -> expr (V4 a) -> expr (V4 a) -> expr (M44 a)

  mkRotation :: expr (m (m (I a))) -> expr (Transform m a u v)
  (<*<) :: expr (Transform m a v w) -> expr (Transform m a u v) -> expr (Transform m a u w)
  (>*>) :: expr (Transform m a u v) -> expr (Transform m a v w) -> expr (Transform m a u w)
  (>*) :: expr (Transform m a u v) -> expr (m (u a)) -> expr (m (v a))

  infixl 7 <*<, >*>
  infixl 7 >*

  (!*!) :: expr (u (v a)) -> expr (v (w a)) -> expr (u (w a))
  (!*) :: expr (u (v a)) -> expr (v a) -> expr (u a)

  infixl 7 !*!, !*

class Vec expr where
  v2 :: expr a -> expr a -> expr (V2 a)
  v3 :: expr a -> expr a -> expr a -> expr (V3 a)
  v4 :: expr a -> expr a -> expr a -> expr a -> expr (V4 a)

  ext3 :: expr (V2 a) -> expr a -> expr (V3 a)
  ext4 :: expr (V3 a) -> expr a -> expr (V4 a)

  norm :: expr (v a) -> expr a

  (^*) :: expr (v a) -> expr a -> expr (v a)
  (*^) :: expr a -> expr (v a) -> expr (v a)
  (^/) :: expr (v a) -> expr a -> expr (v a)

  infixl 7 ^*, *^, ^/

class Scalar expr where
  atan2 :: Unit Length length => expr (length Float) -> expr (length Float) -> expr (I Float)

  float :: expr (scalar a) -> expr (scalar Float)

  min, max :: expr a -> expr a -> expr a

  (<), (>), (<=), (>=), (==) :: expr a -> expr a -> expr Bool

  infix 4 <, >, <=, >=, ==

class Dim expr where
  (.*.) :: expr (u a) -> expr (v a) -> expr (Mul u v a)
  (^*.) :: expr (f (u a)) -> expr (v a) -> expr (f (Mul u v a))
  (.*^) :: expr (u a) -> expr (f (v a)) -> expr (f (Mul u v a))
  (./.) :: expr (u a) -> expr (v a) -> expr (Div u v a)
  (^/.) :: expr (f (u a)) -> expr (v a) -> expr (f (Div u v a))
  (./^) :: expr (u a) -> expr (f (v a)) -> expr (f (Div u v a))

  infixl 7 .*., ^*., .*^, ./., ^/., ./^

class Expr expr where
  let' :: expr a -> (expr a -> expr b) -> expr b

  case' :: expr a -> [(a, expr b)] -> expr b

  lam :: (expr a -> expr b) -> expr (a -> b)
  ($$) :: expr (a -> b) -> (expr a -> expr b)

  infixl 9 $$

  loop :: Integral a => (expr a, expr a) -> (expr a -> expr b) -> expr b

class Boolean expr where
  fromBool :: Bool -> expr Bool
  iff :: expr Bool -> expr a -> expr a -> expr a

  -- arrays

  (!) :: (expr :.: []) a -> expr Int -> expr a

  infixl 9 !

false, true :: Boolean expr => expr Bool
false = fromBool False
true  = fromBool True

class Expr expr => Geom expr where
  primitive :: expr a -> expr a
  vertex :: expr a -> expr a
