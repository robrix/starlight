{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
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
, get
, (^.)
, (^*)
, (^/)
, (!*)
, (!!*)
, (!*!)
, eq
, lt
, gt
, float
, double
, log2
, exp2
, fract
, vec2
, vec3
, vec4
, dvec2
, dvec3
, dvec4
, mat2
, mat3
, mat4
, dmat2
, dmat3
, dmat4
, ext3
, ext4
, dext3
, dext4
, norm
, dot
, lerp
, lerp2
, dFdx
, dFdy
, mod'
, min'
, max'
, atan2'
, texture
, (!)
) where

import Data.Coerce
import Data.Functor.C
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

gl_InstanceID :: Expr 'Vertex Int
gl_InstanceID = var "gl_InstanceID"


gl_Positions :: Expr 'Geometry [V4 Float]
gl_Positions = var "gl_Position"


gl_FragCoord :: Expr 'Fragment (V2 Float)
gl_FragCoord = var "gl_FragCoord"

gl_FrontFacing :: Expr 'Fragment Bool
gl_FrontFacing = var "gl_FrontFacing"

gl_PointCoord :: Expr 'Fragment (V2 Float)
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


var :: String -> Expr k a
var = Expr . pretty

get :: Ref k a -> Expr k a

float :: Expr k a -> Expr k Float
double :: Expr k a -> Expr k Double

vec2 :: [Expr k a] -> Expr k (V2 Float)
vec3 :: [Expr k a] -> Expr k (V3 Float)
vec4 :: [Expr k a] -> Expr k (V4 Float)

dvec2 :: [Expr k a] -> Expr k (V2 Double)
dvec3 :: [Expr k a] -> Expr k (V3 Double)
dvec4 :: [Expr k a] -> Expr k (V4 Double)

mat2 :: [Expr k a] -> Expr k (M22 Float)
mat3 :: [Expr k a] -> Expr k (M33 Float)
mat4 :: [Expr k a] -> Expr k (M44 Float)

dmat2 :: [Expr k a] -> Expr k (M22 Double)
dmat3 :: [Expr k a] -> Expr k (M33 Double)
dmat4 :: [Expr k a] -> Expr k (M44 Double)

ext3 :: Expr k (V2 a) -> Expr k a -> Expr k (V3 Float)
ext4 :: Expr k (V3 a) -> Expr k a -> Expr k (V4 Float)

dext3 :: Expr k (V2 a) -> Expr k a -> Expr k (V3 Double)
dext4 :: Expr k (V3 a) -> Expr k a -> Expr k (V4 Double)

norm :: Expr k (v a) -> Expr k a
dot :: Expr k (v a) -> Expr k (v a) -> Expr k a

(^*) :: Expr k (v a) -> Expr k a -> Expr k (v a)
(^/) :: Expr k (v a) -> Expr k a -> Expr k (v a)
(!*) :: Expr k (v (v a)) -> Expr k (v a) -> Expr k (v a)
(!!*) :: Expr k (v (v a)) -> Expr k a -> Expr k (v (v a))
(!*!) :: Expr k (v (v a)) -> Expr k (v (v a)) -> Expr k (v (v a))
infixl 7 ^*, ^/, !*, !!*, !*!

log2 :: Expr k a -> Expr k a
exp2 :: Expr k a -> Expr k a

lerp :: Expr k a -> Expr k (v a) -> Expr k (v a) -> Expr k (v a)
lerp2 :: Expr k (v a) -> Expr k (v a) -> Expr k (v a) -> Expr k (v a)

dFdx :: Expr k a -> Expr k a
dFdy :: Expr k a -> Expr k a

mod' :: Expr k v -> Expr k v -> Expr k v

min' :: Expr k a -> Expr k a -> Expr k a
max' :: Expr k a -> Expr k a -> Expr k a

atan2' :: Expr k a -> Expr k a -> Expr k a

texture :: Expr k TextureUnit -> Expr k (v Float) -> Expr k (v Float)

fract :: Expr k a -> Expr k a

eq :: Expr k a -> Expr k a -> Expr k Bool
lt :: Expr k a -> Expr k a -> Expr k Bool
gt :: Expr k a -> Expr k a -> Expr k Bool
infix 4 `eq`, `lt`, `gt`

(^.) :: Expr k a -> Prj a b -> Expr k b

(!) :: (Expr k :.: []) a -> Expr k Int -> Expr k a
C v ! n = Expr $ renderExpr v <> brackets (renderExpr n)

infixl 9 !


newtype Expr (k :: Stage) a = Expr { renderExpr :: Doc () }

instance Num (Expr k a) where
  a + b = Expr . parens $ renderExpr a <+> pretty '+' <+> renderExpr b
  a * b = Expr . parens $ renderExpr a <+> pretty '*' <+> renderExpr b
  a - b = Expr . parens $ renderExpr a <+> pretty '-' <+> renderExpr b
  signum a = fn "sign" [ renderExpr a ]
  negate a = Expr . parens $ pretty "-" <> renderExpr a
  abs a = fn "abs" [ renderExpr a ]
  fromInteger i = Expr $ pretty i

instance Fractional (Expr k a) where
  a / b = Expr $ parens $ renderExpr a <+> pretty '/' <+> renderExpr b
  fromRational = lit . fromRational

instance Floating (Expr k a) where
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


get = Expr . renderRef

a ^. Prj s = Expr $ renderExpr a <> pretty s
a ^*  b = Expr . parens $ renderExpr a <+> pretty '*' <+> renderExpr b
a ^/  b = Expr . parens $ renderExpr a <+> pretty '/' <+> renderExpr b
a !*  b = Expr . parens $ renderExpr a <+> pretty '*' <+> renderExpr b
a !!*  b = Expr . parens $ renderExpr a <+> pretty '*' <+> renderExpr b
a !*! b = Expr . parens $ renderExpr a <+> pretty '*' <+> renderExpr b
eq a b = Expr . parens $ renderExpr a <+> pretty "==" <+> renderExpr b
lt a b = Expr . parens $ renderExpr a <+> pretty '<' <+> renderExpr b
gt a b = Expr . parens $ renderExpr a <+> pretty '>' <+> renderExpr b

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


fn :: String -> [Doc ()] -> Expr k b
fn n as = Expr $ pretty n <> tupled as

lit :: Double -> Expr k a
lit = Expr . pretty
