module GL.Shader.GLSL
( GLSL(..)
) where

import Data.Text.Prettyprint.Doc hiding (dot)

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
