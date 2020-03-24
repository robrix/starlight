{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module GL.Shader.GLSL
( GLSL(..)
) where

import qualified Data.Text.Prettyprint.Doc as Doc

newtype GLSL a = GLSL { renderGLSL :: Doc.Doc () }
  deriving (Monoid, Semigroup)

instance Num (GLSL a) where
  a + b = parens $ a <+> pretty '+' <+> b
  a * b = parens $ a <+> pretty '*' <+> b
  a - b = parens $ a <+> pretty '-' <+> b
  signum = fn "sign"
  negate a = parens $ pretty "-" <> a
  abs = fn "abs"
  fromInteger = pretty

instance Fractional (GLSL a) where
  a / b = parens $ a <+> pretty '/' <+> b
  fromRational = lit . fromRational

instance Floating (GLSL a) where
  exp = fn "exp"
  log = fn "log"
  sqrt = fn "sqrt"
  a ** b = fn "pow" a b
  sin = fn "sin"
  cos = fn "cos"
  tan = fn "tan"
  asin = fn "asin"
  acos = fn "acos"
  atan = fn "atan"
  sinh = fn "sinh"
  cosh = fn "cosh"
  tanh = fn "tanh"
  asinh = fn "asinh"
  acosh = fn "acosh"
  atanh = fn "atanh"
  pi = lit pi

fn :: Fn a => String -> a
fn n = fn' n id

lit :: Double -> GLSL a
lit = pretty

pretty :: Doc.Pretty a => a -> GLSL b
pretty = GLSL . Doc.pretty

parens :: GLSL a -> GLSL a
parens (GLSL a) = GLSL (Doc.parens a)

(<+>) :: GLSL a -> GLSL b -> GLSL c
GLSL a <+> GLSL b = GLSL (a Doc.<+> b)

infixr 6 <+>


class Fn a where
  fn' :: String -> ([Doc.Doc ()] -> [Doc.Doc ()]) -> a

instance Fn b => Fn (GLSL a -> b) where
  fn' s accum (GLSL a) = fn' s (accum . (a:))

instance Fn (GLSL a) where
  fn' s accum = pretty s <> GLSL (Doc.tupled (accum []))
