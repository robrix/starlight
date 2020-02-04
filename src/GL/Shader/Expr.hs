{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
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
, Expr(Var)
, get
  -- ** Constructors
, float
, double
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
-- ** Vector operations
, ext3
, ext4
, dext3
, dext4
, norm
, dot
, (^*)
, (!*)
, (!!*)
, (!*!)
-- ** General operations
, log2
, exp2
, lerp
, lerp2
, dFdx
, mod'
, min'
, max'
, atan2'
, texture
, coerce
  -- ** Comparison
, eq
, lt
, gt
  -- ** Projection expressions
, (^.)
, (^^.)
  -- ** Pretty-printing
, renderExpr
) where

import qualified Data.Coerce as C
import           Data.Text.Prettyprint.Doc hiding (dot)
import           GL.Shader (Type(..))
import           GL.TextureUnit
import           Linear.Matrix (M22, M33, M44)
import           Linear.V2 (V2(..))
import           Linear.V3 (V3(..))
import           Linear.V4 (V4(..))

data Ref (k :: Type) t
  = Ref String
  | forall s . Ref k s :^^. Prj s t

gl_Position :: Ref k (V4 Float)
gl_Position = Ref "gl_Position"

gl_PointSize :: Ref 'Vertex Float
gl_PointSize = Ref "gl_PointSize"

gl_InstanceID :: Expr 'Vertex Int
gl_InstanceID = Var "gl_InstanceID"


gl_Positions :: Expr 'Geometry [V4 Float]
gl_Positions = Var "gl_Position"


gl_FragCoord :: Expr 'Fragment (V2 Float)
gl_FragCoord = Var "gl_FragCoord"

gl_FrontFacing :: Expr 'Fragment Bool
gl_FrontFacing = Var "gl_FrontFacing"

gl_PointCoord :: Expr 'Fragment (V2 Float)
gl_PointCoord = Var "gl_PointCoord"


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


data Expr (k :: Type) a where
  Var :: String -> Expr k a
  Lit :: Double -> Expr k a

  (:+) :: Expr k a -> Expr k a -> Expr k a
  (:*) :: Expr k a -> Expr k a -> Expr k a
  (:-) :: Expr k a -> Expr k a -> Expr k a
  Signum :: Expr k a -> Expr k a
  Negate :: Expr k a -> Expr k a
  Abs :: Expr k a -> Expr k a
  FromInteger :: Integer -> Expr k a

  (:/) :: Expr k a -> Expr k a -> Expr k a

  Fn :: String -> [Expr k a] -> Expr k b
  Exp :: Expr k a -> Expr k a
  Log :: Expr k a -> Expr k a
  Sqrt :: Expr k a -> Expr k a
  (:**) :: Expr k a -> Expr k a -> Expr k a
  Sin :: Expr k a -> Expr k a
  Cos :: Expr k a -> Expr k a
  Tan :: Expr k a -> Expr k a
  ASin :: Expr k a -> Expr k a
  ACos :: Expr k a -> Expr k a
  ATan :: Expr k a -> Expr k a
  SinH :: Expr k a -> Expr k a
  CosH :: Expr k a -> Expr k a
  TanH :: Expr k a -> Expr k a
  ASinH :: Expr k a -> Expr k a
  ACosH :: Expr k a -> Expr k a
  ATanH :: Expr k a -> Expr k a

  (:^.) :: Expr k a -> Prj a b -> Expr k b
  (:^*) :: Expr k (v a) -> Expr k a -> Expr k (v a)
  (:!*) :: Expr k (v (v a)) -> Expr k (v a) -> Expr k (v a)
  (:!!*) :: Expr k (v (v a)) -> Expr k a -> Expr k (v (v a))
  (:!*!) :: Expr k (v (v a)) -> Expr k (v (v a)) -> Expr k (v (v a))

  Eq :: Expr k a -> Expr k a -> Expr k Bool
  Lt :: Expr k a -> Expr k a -> Expr k Bool
  Gt :: Expr k a -> Expr k a -> Expr k Bool

  Get :: Ref k a -> Expr k a

  Float :: Expr k a -> Expr k Float
  Double :: Expr k a -> Expr k Double
  Vec :: String -> Int -> [Expr k a] -> Expr k (v b)
  Mat :: String -> Int -> [Expr k a] -> Expr k (v (v b))
  Ext3 :: String -> Expr k (V2 a) -> Expr k a -> Expr k (V3 b)
  Ext4 :: String -> Expr k (V3 a) -> Expr k a -> Expr k (V4 b)
  Norm :: Expr k (v a) -> Expr k a
  Dot :: Expr k (v a) -> Expr k (v a) -> Expr k (v a)
  Lerp :: Expr k a -> Expr k (v a) -> Expr k (v a) -> Expr k (v a)
  Lerp2 :: Expr k (v a) -> Expr k (v a) -> Expr k (v a) -> Expr k (v a)
  Dfdx :: Expr k a -> Expr k a
  Mod :: Expr k a -> Expr k a -> Expr k a
  Min :: Expr k a -> Expr k a -> Expr k a
  Max :: Expr k a -> Expr k a -> Expr k a
  Atan2 :: Expr k a -> Expr k a -> Expr k a
  Texture :: Expr k TextureUnit -> Expr k (v Float) -> Expr k (v Float)

  Coerce :: C.Coercible a b => Expr k a -> Expr k b

infixl 6 :+
infixl 7 :*
infixl 6 :-
infixl 7 :/
infixr 8 :**
infixl 8 :^.
infixl 7 :^*
infixl 7 :!*
infixl 7 :!!*
infixl 7 :!*!

instance Num (Expr k a) where
  (+) = (:+)
  (*) = (:*)
  (-) = (:-)
  signum = Signum
  negate = Negate
  abs = Abs
  fromInteger = FromInteger

instance Fractional (Expr k a) where
  (/) = (:/)
  fromRational = Lit . fromRational

instance Floating (Expr k a) where
  pi = Lit pi
  exp = Exp
  log = Log
  sqrt = Sqrt
  (**) = (:**)
  sin = Sin
  cos = Cos
  tan = Tan
  asin = ASin
  acos = ACos
  atan = ATan
  sinh = SinH
  cosh = CosH
  tanh = TanH
  asinh = ASinH
  acosh = ACosH
  atanh = ATanH

get :: Ref k a -> Expr k a
get = Get


float :: Expr k a -> Expr k Float
float = Float

double :: Expr k a -> Expr k Double
double = Double


vec2 :: [Expr k a] -> Expr k (V2 Float)
vec2 = Vec "" 2

vec3 :: [Expr k a] -> Expr k (V3 Float)
vec3 = Vec "" 3

vec4 :: [Expr k a] -> Expr k (V4 Float)
vec4 = Vec "" 4


dvec2 :: [Expr k a] -> Expr k (V2 Double)
dvec2 = Vec "d" 2

dvec3 :: [Expr k a] -> Expr k (V3 Double)
dvec3 = Vec "d" 3

dvec4 :: [Expr k a] -> Expr k (V4 Double)
dvec4 = Vec "d" 4


mat2 :: [Expr k a] -> Expr k (M22 Float)
mat2 = Mat "" 2

mat3 :: [Expr k a] -> Expr k (M33 Float)
mat3 = Mat "" 3

mat4 :: [Expr k a] -> Expr k (M44 Float)
mat4 = Mat "" 4


dmat2 :: [Expr k a] -> Expr k (M22 Double)
dmat2 = Mat "d" 2

dmat3 :: [Expr k a] -> Expr k (M33 Double)
dmat3 = Mat "d" 3

dmat4 :: [Expr k a] -> Expr k (M44 Double)
dmat4 = Mat "d" 4


ext3 :: Expr k (V2 a) -> Expr k a -> Expr k (V3 Float)
ext3 = Ext3 ""

ext4 :: Expr k (V3 a) -> Expr k a -> Expr k (V4 Float)
ext4 = Ext4 ""


dext3 :: Expr k (V2 a) -> Expr k a -> Expr k (V3 Double)
dext3 = Ext3 "d"

dext4 :: Expr k (V3 a) -> Expr k a -> Expr k (V4 Double)
dext4 = Ext4 "d"


norm :: Expr k (v a) -> Expr k a
norm = Norm

dot :: Expr k (v a) -> Expr k (v a) -> Expr k (v a)
dot = Dot

(^*) :: Expr k (v a) -> Expr k a -> Expr k (v a)
(^*) = (:^*)

infixl 7 ^*

(!*) :: Expr k (v (v a)) -> Expr k (v a) -> Expr k (v a)
(!*) = (:!*)

infixl 7 !*

(!!*) :: Expr k (v (v a)) -> Expr k a -> Expr k (v (v a))
(!!*) = (:!!*)

infixl 7 !!*

(!*!) :: Expr k (v (v a)) -> Expr k (v (v a)) -> Expr k (v (v a))
(!*!) = (:!*!)

infixl 7 !*!


log2 :: Expr k a -> Expr k a
log2 a = Fn "log2" [a]

exp2 :: Expr k a -> Expr k a
exp2 a = Fn "exp2" [a]

lerp :: Expr k a -> Expr k (v a) -> Expr k (v a) -> Expr k (v a)
lerp = Lerp

lerp2 :: Expr k (v a) -> Expr k (v a) -> Expr k (v a) -> Expr k (v a)
lerp2 = Lerp2

dFdx :: Expr k a -> Expr k a
dFdx = Dfdx

mod' :: Expr k v -> Expr k v -> Expr k v
mod' = Mod

min' :: Expr k a -> Expr k a -> Expr k a
min' = Min

max' :: Expr k a -> Expr k a -> Expr k a
max' = Max

atan2' :: Expr k a -> Expr k a -> Expr k a
atan2' = Atan2

texture :: Expr k TextureUnit -> Expr k (v Float) -> Expr k (v Float)
texture = Texture


coerce :: C.Coercible a b => Expr k a -> Expr k b
coerce = Coerce


eq :: Expr k a -> Expr k a -> Expr k Bool
eq = Eq

infix 4 `eq`

lt :: Expr k a -> Expr k a -> Expr k Bool
lt = Lt

infix 4 `lt`

gt :: Expr k a -> Expr k a -> Expr k Bool
gt = Gt

infix 4 `gt`


(^.) :: Expr k a -> Prj a b -> Expr k b
(^.) = (:^.)

infixl 8 ^.

(^^.) :: Ref k a -> Prj a b -> Ref k b
(^^.) = (:^^.)

infixl 8 ^^.


renderExpr :: Expr k a -> Doc ()
renderExpr = \case
  Var n -> pretty n
  Lit d -> pretty d
  a :+ b -> parens $ renderExpr a <+> pretty '+' <+> renderExpr b
  a :* b -> parens $ renderExpr a <+> pretty '*' <+> renderExpr b
  a :- b -> parens $ renderExpr a <+> pretty '-' <+> renderExpr b
  a :/ b -> parens $ renderExpr a <+> pretty '/' <+> renderExpr b
  Signum a -> fn "sign" [renderExpr a]
  Negate a -> parens $ pretty "-" <> renderExpr a
  Abs a -> fn "abs" [renderExpr a]
  FromInteger i -> pretty i
  Fn s as -> fn s (map renderExpr as)
  Exp a -> fn "exp" [renderExpr a]
  Log a -> fn "log" [renderExpr a]
  Sqrt a -> fn "sqrt" [renderExpr a]
  a :** b -> fn "pow" [renderExpr a, renderExpr b]
  Sin a -> fn "sin" [renderExpr a]
  Cos a -> fn "cos" [renderExpr a]
  Tan a -> fn "tan" [renderExpr a]
  ASin a -> fn "asin" [renderExpr a]
  ACos a -> fn "acos" [renderExpr a]
  ATan a -> fn "atan" [renderExpr a]
  SinH a -> fn "sinh" [renderExpr a]
  CosH a -> fn "cosh" [renderExpr a]
  TanH a -> fn "tanh" [renderExpr a]
  ASinH a -> fn "asinh" [renderExpr a]
  ACosH a -> fn "acosh" [renderExpr a]
  ATanH a -> fn "atanh" [renderExpr a]
  a :^. Prj s -> renderExpr a <> pretty s
  a :^*  b -> parens $ renderExpr a <+> pretty '*' <+> renderExpr b
  a :!*  b -> parens $ renderExpr a <+> pretty '*' <+> renderExpr b
  a :!!*  b -> parens $ renderExpr a <+> pretty '*' <+> renderExpr b
  a :!*! b -> parens $ renderExpr a <+> pretty '*' <+> renderExpr b
  Eq a b -> parens $ renderExpr a <+> pretty "==" <+> renderExpr b
  Lt a b -> parens $ renderExpr a <+> pretty '<' <+> renderExpr b
  Gt a b -> parens $ renderExpr a <+> pretty '>' <+> renderExpr b
  Get r -> renderRef r
  Float a -> fn "float" [renderExpr a]
  Double a -> fn "double" [renderExpr a]
  Vec p n as -> fn (p <> "vec" <> show n) (map renderExpr as)
  Mat p n as -> fn (p <> "mat" <> show n) (map renderExpr as)
  Ext3 p a b -> fn (p <> "vec3") [renderExpr a, renderExpr b]
  Ext4 p a b -> fn (p <> "vec4") [renderExpr a, renderExpr b]
  Norm a -> fn "length" [renderExpr a]
  Dot a b -> fn "dot" [renderExpr a, renderExpr b]
  Lerp t a b -> fn "mix" [renderExpr a, renderExpr b, renderExpr t]
  Lerp2 t a b -> fn "mix" [renderExpr a, renderExpr b, renderExpr t]
  Dfdx a -> fn "dFdx" [renderExpr a]
  Mod a b -> fn "mod" [renderExpr a, renderExpr b]
  Min a b -> fn "min" [renderExpr a, renderExpr b]
  Max a b -> fn "max" [renderExpr a, renderExpr b]
  Atan2 a b -> fn "atan" [renderExpr a, renderExpr b]
  Texture a b -> fn "texture" [renderExpr a, renderExpr b]
  Coerce a -> renderExpr a
  where
  fn n as = pretty n <> tupled as
