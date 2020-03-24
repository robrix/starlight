{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# HLINT ignore "Use camelCase" #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
module GL.Shader.Expr
( -- * References (mutable variables)
  Ref(..)
, gl_Position
, gl_PointSize
, gl_InstanceID
, gl_Positions
, gl_FragCoord
, gl_FrontFacing
, gl_PointCoord
, (^^.)
  -- ** Pretty-printing
, renderRef

  -- * Projections
, Prj
, _x
, _y
, _z
, _w
, _xy
, _yz
, _xz
, _xw
, _zw
, _xyz
, _xywz
, _a
, ix

  -- * Expressions
, Expr(..)
  -- ** Pretty-printing
, Render(..)
-- , renderExpr
) where

import Data.Coerce
import Data.Kind (Type)
import Data.Text.Prettyprint.Doc hiding (dot)
import GL.Shader (Stage(..))
import GL.TextureUnit
import Linear.Matrix (M22, M33, M44)
import Linear.V2 (V2(..))
import Linear.V3 (V3(..))
import Linear.V4 (V4(..))

data Ref (k :: Stage) t
  = Ref String
  | forall s . Ref k s :^^. Prj s t

gl_Position :: Ref k (V4 Float)
gl_Position = Ref "gl_Position"

gl_PointSize :: Ref 'Vertex Float
gl_PointSize = Ref "gl_PointSize"

gl_InstanceID :: Expr expr => expr 'Vertex Int
gl_InstanceID = var "gl_InstanceID"


gl_Positions :: Expr expr => expr 'Geometry [V4 Float]
gl_Positions = var "gl_Position"


gl_FragCoord :: Expr expr => expr 'Fragment (V2 Float)
gl_FragCoord = var "gl_FragCoord"

gl_FrontFacing :: Expr expr => expr 'Fragment Bool
gl_FrontFacing = var "gl_FrontFacing"

gl_PointCoord :: Expr expr => expr 'Fragment (V2 Float)
gl_PointCoord = var "gl_PointCoord"


(^^.) :: Ref k a -> Prj a b -> Ref k b
(^^.) = (:^^.)

infixl 8 ^^.


renderRef :: Ref k a -> Doc ()
renderRef = \case
  Ref n        -> pretty n
  r :^^. Prj p -> renderRef r <> pretty p


newtype Prj s t = Prj String

_x :: Prj (v a) a
_x = Prj ".x"

_y :: Prj (v a) a
_y = Prj ".y"

_z :: Prj (v a) a
_z = Prj ".z"

_w :: Prj (v a) a
_w = Prj ".w"

_xy :: Prj (v a) (V2 a)
_xy = Prj ".xy"

_yz :: Prj (v a) (V2 a)
_yz = Prj ".yz"

_xz :: Prj (v a) (V2 a)
_xz = Prj ".xz"

_xw :: Prj (v a) (V2 a)
_xw = Prj ".xw"

_zw :: Prj (v a) (V2 a)
_zw = Prj ".zw"

_xyz :: Prj (v a) (V3 a)
_xyz = Prj ".xyz"

_xywz :: Prj (v a) (V4 a)
_xywz = Prj ".xywz"

_a :: Prj (v a) a
_a = Prj ".a"

ix :: Int -> Prj [a] a
ix i = Prj ("[" <> show i <> "]")


class Expr (expr :: Stage -> Type -> Type) where
  var :: String -> expr k a

  get :: Ref k a -> expr k a

  float :: expr k a -> expr k Float
  double :: expr k a -> expr k Double

  vec2 :: [expr k a] -> expr k (V2 Float)
  vec3 :: [expr k a] -> expr k (V3 Float)
  vec4 :: [expr k a] -> expr k (V4 Float)

  dvec2 :: [expr k a] -> expr k (V2 Double)
  dvec3 :: [expr k a] -> expr k (V3 Double)
  dvec4 :: [expr k a] -> expr k (V4 Double)

  mat2 :: [expr k a] -> expr k (M22 Float)
  mat3 :: [expr k a] -> expr k (M33 Float)
  mat4 :: [expr k a] -> expr k (M44 Float)

  dmat2 :: [expr k a] -> expr k (M22 Double)
  dmat3 :: [expr k a] -> expr k (M33 Double)
  dmat4 :: [expr k a] -> expr k (M44 Double)

  ext3 :: expr k (V2 a) -> expr k a -> expr k (V3 Float)
  ext4 :: expr k (V3 a) -> expr k a -> expr k (V4 Float)

  dext3 :: expr k (V2 a) -> expr k a -> expr k (V3 Double)
  dext4 :: expr k (V3 a) -> expr k a -> expr k (V4 Double)

  norm :: expr k (v a) -> expr k a
  dot :: expr k (v a) -> expr k (v a) -> expr k a

  (^*) :: expr k (v a) -> expr k a -> expr k (v a)
  (^/) :: expr k (v a) -> expr k a -> expr k (v a)
  (!*) :: expr k (v (v a)) -> expr k (v a) -> expr k (v a)
  (!!*) :: expr k (v (v a)) -> expr k a -> expr k (v (v a))
  (!*!) :: expr k (v (v a)) -> expr k (v (v a)) -> expr k (v (v a))
  infixl 7 ^*, ^/, !*, !!*, !*!

  log2 :: expr k a -> expr k a
  exp2 :: expr k a -> expr k a

  lerp :: expr k a -> expr k (v a) -> expr k (v a) -> expr k (v a)
  lerp2 :: expr k (v a) -> expr k (v a) -> expr k (v a) -> expr k (v a)

  dFdx :: expr k a -> expr k a
  dFdy :: expr k a -> expr k a

  mod' :: expr k v -> expr k v -> expr k v

  min' :: expr k a -> expr k a -> expr k a
  max' :: expr k a -> expr k a -> expr k a

  atan2' :: expr k a -> expr k a -> expr k a

  texture :: expr k TextureUnit -> expr k (v Float) -> expr k (v Float)

  fract :: expr k a -> expr k a

  eq :: expr k a -> expr k a -> expr k Bool
  lt :: expr k a -> expr k a -> expr k Bool
  gt :: expr k a -> expr k a -> expr k Bool
  infix 4 `eq`, `lt`, `gt`

  (^.) :: expr k a -> Prj a b -> expr k b


newtype Render (k :: Stage) a = Render { renderExpr :: Doc () }

instance Num (Render k a) where
  a + b = Render . parens $ renderExpr a <+> pretty '+' <+> renderExpr b
  a * b = Render . parens $ renderExpr a <+> pretty '*' <+> renderExpr b
  a - b = Render . parens $ renderExpr a <+> pretty '-' <+> renderExpr b
  signum a = fn "sign" [ renderExpr a ]
  negate a = Render . parens $ pretty "-" <> renderExpr a
  abs a = fn "abs" [ renderExpr a ]
  fromInteger i = Render $ pretty i

instance Fractional (Render k a) where
  a / b = Render $ parens $ renderExpr a <+> pretty '/' <+> renderExpr b
  fromRational = lit . fromRational

instance Floating (Render k a) where
  exp a = fn "exp" [ renderExpr a ]
  log a = fn "log" [ renderExpr a ]
  sqrt a = fn "sqrt" [ renderExpr a ]
  a ** b = fn "pow" [ renderExpr a, renderExpr b ]
  sin a = fn "sin" [ renderExpr a ]
  cos a = fn "cos" [ renderExpr a ]
  tan a = fn "tan" [ renderExpr a ]
  asin a = fn "asin" [ renderExpr a ]
  acos a = fn "acos" [ renderExpr a ]
  atan a = fn "atan" [ renderExpr a ]
  sinh a = fn "sinh" [ renderExpr a ]
  cosh a = fn "cosh" [ renderExpr a ]
  tanh a = fn "tanh" [ renderExpr a ]
  asinh a = fn "asinh" [ renderExpr a ]
  acosh a = fn "acosh" [ renderExpr a ]
  atanh a = fn "atanh" [ renderExpr a ]
  pi = lit pi


instance Expr Render where
  var = Render . pretty
  get = Render . renderRef

  a ^. Prj s = Render $ renderExpr a <> pretty s
  a ^*  b = Render . parens $ renderExpr a <+> pretty '*' <+> renderExpr b
  a ^/  b = Render . parens $ renderExpr a <+> pretty '/' <+> renderExpr b
  a !*  b = Render . parens $ renderExpr a <+> pretty '*' <+> renderExpr b
  a !!*  b = Render . parens $ renderExpr a <+> pretty '*' <+> renderExpr b
  a !*! b = Render . parens $ renderExpr a <+> pretty '*' <+> renderExpr b
  eq a b = Render . parens $ renderExpr a <+> pretty "==" <+> renderExpr b
  lt a b = Render . parens $ renderExpr a <+> pretty '<' <+> renderExpr b
  gt a b = Render . parens $ renderExpr a <+> pretty '>' <+> renderExpr b

  float a = fn "float" [ renderExpr a ]
  double a = fn "double" [ renderExpr a ]

  log2 = fn "log2" . pure . coerce
  exp2 = fn "exp2" . pure . coerce
  fract = fn "fract" . pure . coerce

  vec2 = fn "vec2" . coerce
  vec3 = fn "vec3" . coerce
  vec4 = fn "vec4" . coerce

  dvec2 = fn "dvec2" . coerce
  dvec3 = fn "dvec3" . coerce
  dvec4 = fn "dvec4" . coerce

  mat2 = fn "mat2" . coerce
  mat3 = fn "mat3" . coerce
  mat4 = fn "mat4" . coerce

  dmat2 = fn "dmat2" . coerce
  dmat3 = fn "dmat3" . coerce
  dmat4 = fn "dmat4" . coerce

  ext3 a b = fn "vec3" [ renderExpr a, renderExpr b ]
  ext4 a b = fn "vec4" [ renderExpr a, renderExpr b ]

  dext3 a b = fn "dvec3" [ renderExpr a, renderExpr b ]
  dext4 a b = fn "dvec4" [ renderExpr a, renderExpr b ]

  norm a = fn "length" [ renderExpr a ]
  dot a b = fn "dot" [ renderExpr a, renderExpr b ]

  lerp  t a b = fn "mix" [ renderExpr a, renderExpr b, renderExpr t ]
  lerp2 t a b = fn "mix" [ renderExpr a, renderExpr b, renderExpr t ]

  dFdx a = fn "dFdx" [ renderExpr a ]
  dFdy a = fn "dFdy" [ renderExpr a ]

  mod' a b = fn "mod" [ renderExpr a, renderExpr b ]
  min' a b = fn "min" [ renderExpr a, renderExpr b ]
  max' a b = fn "max" [ renderExpr a, renderExpr b ]
  atan2' a b = fn "atan" [ renderExpr a, renderExpr b ]

  texture a b = fn "texture" [ renderExpr a, renderExpr b ]


fn :: String -> [Doc ()] -> Render k b
fn n as = Render $ pretty n <> tupled as

lit :: Double -> Render k a
lit = Render . pretty
