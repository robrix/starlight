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
  signum a = fn "sign" [ renderGLSL a ]
  negate a = parens $ pretty "-" <> a
  abs a = fn "abs" [ renderGLSL a ]
  fromInteger = pretty

instance Fractional (GLSL a) where
  a / b = parens $ a <+> pretty '/' <+> b
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

fn :: String -> [Doc.Doc ()] -> GLSL b
fn n as = pretty n <> GLSL (Doc.tupled as)

lit :: Double -> GLSL a
lit = pretty

pretty :: Doc.Pretty a => a -> GLSL b
pretty = GLSL . Doc.pretty

parens :: GLSL a -> GLSL a
parens (GLSL a) = GLSL (Doc.parens a)

(<+>) :: GLSL a -> GLSL b -> GLSL c
GLSL a <+> GLSL b = GLSL (a Doc.<+> b)

infixr 6 <+>
