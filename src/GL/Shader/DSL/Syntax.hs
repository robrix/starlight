{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
module GL.Shader.DSL.Syntax
( Expr(..)
, false
, true
, Geom(..)
, GLSL(..)
) where

import Data.Functor.C
import Data.Functor.I
import Data.Text.Prettyprint.Doc hiding (dot)
import Geometry.Transform (Transform)
import Linear.Matrix
import Linear.V2
import Linear.V3
import Linear.V4
import Unit.Algebra (Div, Mul)
import Unit (Unit)
import Unit.Length (Length)

class Expr expr where
  -- matrix

  m2 :: expr (V2 a) -> expr (V2 a) -> expr (M22 a)
  m3 :: expr (V3 a) -> expr (V3 a) -> expr (V3 a) -> expr (M33 a)
  m4 :: expr (V4 a) -> expr (V4 a) -> expr (V4 a) -> expr (V4 a) -> expr (M44 a)

  mkRotation :: expr (m (m (I a))) -> expr (Transform m a u v)
  (<<<) :: expr (Transform m a v w) -> expr (Transform m a u v) -> expr (Transform m a u w)
  (>>>) :: expr (Transform m a u v) -> expr (Transform m a v w) -> expr (Transform m a u w)
  (>*) :: expr (Transform m a u v) -> expr (m (v a)) -> expr (m (u a))

  infixr 1 <<<, >>>
  infixl 7 >*

  (!*!) :: expr (u (v a)) -> expr (v (w a)) -> expr (u (w a))
  (!*) :: expr (u (v a)) -> expr (v a) -> expr (u a)

  infixl 7 !*!, !*

  -- vector

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

  -- scalar

  atan2 :: Unit Length length => expr (length Float) -> expr (length Float) -> expr (I Float)

  float :: expr (scalar a) -> expr (scalar Float)

  min, max :: expr a -> expr a -> expr a

  (<), (>), (<=), (>=), (==) :: expr a -> expr a -> expr Bool

  infix 4 <, >, <=, >=, ==

  -- units

  (.*.) :: expr (u a) -> expr (v a) -> expr (Mul u v a)
  (^*.) :: expr (f (u a)) -> expr (v a) -> expr (f (Mul u v a))
  (.*^) :: expr (u a) -> expr (f (v a)) -> expr (f (Mul u v a))
  (./.) :: expr (u a) -> expr (v a) -> expr (Div u v a)
  (^/.) :: expr (f (u a)) -> expr (v a) -> expr (f (Div u v a))
  (./^) :: expr (u a) -> expr (f (v a)) -> expr (f (Div u v a))

  infixl 7 .*., ^*., .*^, ./., ^/., ./^

  -- general syntax

  let' :: expr a -> (expr a -> expr b) -> expr b

  case' :: expr a -> [(a, expr b)] -> expr b

  lam :: (expr a -> expr b) -> expr (a -> b)
  ($$) :: expr (a -> b) -> (expr a -> expr b)

  infixl 9 $$

  loop :: (expr Int, expr Int) -> (expr a -> expr b) -> expr b

  -- booleans

  fromBool :: Bool -> expr Bool
  iff :: expr Bool -> expr a -> expr a -> expr a

  -- arrays

  (!) :: (expr :.: []) a -> expr Int -> expr a

  infixl 9 !

false, true :: Expr expr => expr Bool
false = fromBool False
true  = fromBool True

class Expr expr => Geom expr where
  primitive :: expr a -> expr a
  vertex :: expr a -> expr a


newtype GLSL a = GLSL { renderGLSL :: Doc () }

instance Num (GLSL a) where
  a + b = GLSL . parens $ renderGLSL a <+> pretty '+' <+> renderGLSL b
  a * b = GLSL . parens $ renderGLSL a <+> pretty '*' <+> renderGLSL b
  a - b = GLSL . parens $ renderGLSL a <+> pretty '-' <+> renderGLSL b
  signum a = fn "sign" [ renderGLSL a ]
  negate a = GLSL . parens $ pretty "-" <> renderGLSL a
  abs a = fn "abs" [ renderGLSL a ]
  fromInteger i = GLSL $ pretty i

instance Fractional (GLSL a) where
  a / b = GLSL $ parens $ renderGLSL a <+> pretty '/' <+> renderGLSL b
  fromRational = lit . fromRational

instance Floating (GLSL a) where
  exp a = fn "exp" [ renderGLSL a ]
  log a = fn "log" [ renderGLSL a ]
  sqrt a = fn "sqrt" [ renderGLSL a ]
  a ** b = fn "pow" [ renderGLSL a, renderGLSL b ]
  sin a = fn "sin" [ renderGLSL a ]
  cos a = fn "cos" [ renderGLSL a ]
  tan a = fn "tan" [ renderGLSL a ]
  asin a = fn "asin" [ renderGLSL a ]
  acos a = fn "acos" [ renderGLSL a ]
  atan a = fn "atan" [ renderGLSL a ]
  sinh a = fn "sinh" [ renderGLSL a ]
  cosh a = fn "cosh" [ renderGLSL a ]
  tanh a = fn "tanh" [ renderGLSL a ]
  asinh a = fn "asinh" [ renderGLSL a ]
  acosh a = fn "acosh" [ renderGLSL a ]
  atanh a = fn "atanh" [ renderGLSL a ]
  pi = lit pi

fn :: String -> [Doc ()] -> GLSL b
fn n as = GLSL $ pretty n <> tupled as

lit :: Double -> GLSL a
lit = GLSL . pretty
