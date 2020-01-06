{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module GL.Shader.DSL
( Shader
, program
, Stage
, vertex
, fragment
, (Cat.>>>)
, None(..)
, shaderSources
, Stmt
, Expr
, Ref
, Prj
, let'
, var
, get
, float
, vec2
, vec3
, vec4
, mat2
, mat3
, mat4
, ext3
, ext4
, norm
, dot
, lerp
, lerp2
, dFdx
, mod'
, min'
, max'
, atan2'
, texture
, coerce
, gl_Position
, gl_PointSize
, gl_InstanceID
, gl_FragCoord
, gl_FrontFacing
, gl_PointCoord
, discard
, iff
, switch
, break
, while
, eq
, lt
, gt
, (.=)
, (+=)
, (*=)
, (*!=)
, (^.)
, (^^.)
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
, (^*)
, (!*)
, (!*!)
, renderStmt
, renderExpr
  -- * Re-exports
, Fields(..)
, Vars
, Type(..)
, Colour
, M22
, M33
, M44
, Point(..)
, TextureUnit
, V2
, V3
, V4
) where

import qualified Control.Category as Cat
import           Control.Monad (ap, liftM, (<=<))
import qualified Data.Coerce as C
import           Data.Function (fix)
import           Data.Functor.Const
import           Data.Text.Prettyprint.Doc hiding (dot)
import           Data.Text.Prettyprint.Doc.Render.String
import           GHC.Generics
import           GL.Shader (Type(..))
import           GL.Shader.Vars
import           GL.TextureUnit
import qualified GL.Uniform as GL
import           Linear.Affine (Point(..))
import           Linear.Matrix (M22, M33, M44)
import           Linear.V2 (V2(..))
import           Linear.V3 (V3(..))
import           Linear.V4 (V4(..))
import           Prelude hiding (break)
import           UI.Colour (Colour)

data Shader (u :: (* -> *) -> *) (i :: (* -> *) -> *) (o :: (* -> *) -> *) where
  Shader :: Vars u => ((forall k . u (Expr k)) -> Stage i o) -> Shader u i o

program :: Vars u => ((forall k . u (Expr k)) -> Stage i o) -> Shader u i o
program = Shader


data Stage i o where
  Id :: Stage i i
  (:>>>) :: Stage i x -> Stage x o -> Stage i o
  V :: (Vars i, Vars o) => (i (Expr 'Vertex)   -> o (Ref 'Vertex)   -> Stmt 'Vertex   ()) -> Stage i o
  F :: (Vars i, Vars o) => (i (Expr 'Fragment) -> o (Ref 'Fragment) -> Stmt 'Fragment ()) -> Stage i o

vertex   :: (Vars i, Vars o) => (i (Expr 'Vertex)   -> o (Ref 'Vertex)   -> Stmt 'Vertex   ()) -> Stage i o
vertex = V

fragment :: (Vars i, Vars o) => (i (Expr 'Fragment) -> o (Ref 'Fragment) -> Stmt 'Fragment ()) -> Stage i o
fragment = F

instance Cat.Category Stage where
  id = Id
  (.) = flip (:>>>)


data None (v :: * -> *) = None
  deriving (Generic)

instance Vars None

shaderSources :: Shader u i o -> [(Type, String)]
shaderSources (Shader f) = fmap (renderString . layoutPretty defaultLayoutOptions) <$> stageSources u' (f u) where
  u = makeVars (Var . name)
  u' = foldVars (getConst . value) (makeVars (pvar "uniform" . name) `like` u)

stageSources :: Doc () -> Stage i o -> [(Type, Doc ())]
stageSources u = \case
  Id  -> []
  V s -> [(Vertex,   renderStage s)]
  F s -> [(Fragment, renderStage s)]
  l :>>> r -> stageSources u l <> stageSources u r
  where
  renderStage :: (Vars i, Vars o) => (i (Expr k) -> o (Ref k) -> Stmt k ()) -> Doc ()
  renderStage f
    =  pretty "#version 410" <> hardline
    <> u
    <> foldVars (getConst . value) (makeVars (pvar "in"      . name) `like` i)
    <> foldVars (getConst . value) (makeVars (pvar "out"     . name) `like` o)
    <> pretty "void" <+> pretty "main" <> parens mempty <+> braces (nest 2 (line <> renderStmt (f i o) <> line)) where
    i = makeVars (Var . name)
    o = makeVars (Ref . name)

like :: t (Const a) -> t b -> t (Const a)
like = const

pvar :: GL.Uniform a => String -> String -> Const (Doc ()) a
pvar qual n = fix $ \ c -> Const $ pretty qual <+> renderTypeOf c <+> pretty n <> pretty ';' <> hardline

renderTypeOf :: forall a expr . GL.Uniform a => expr a -> Doc ()
renderTypeOf _ = pretty (GL.glslType @a)


data Stmt (k :: Type) a where
  Pure :: a -> Stmt k a
  Let :: GL.Uniform b => String -> Expr k b -> (Const String b -> Stmt k a) -> Stmt k a
  Discard :: Stmt 'Fragment a -> Stmt 'Fragment a
  If :: Expr k Bool -> Stmt k () -> Stmt k () -> Stmt k a -> Stmt k a
  Switch :: Expr k Int -> [(Maybe Int, Stmt k ())] -> Stmt k a -> Stmt k a
  Break :: Stmt k a -> Stmt k a
  While :: Expr k Bool -> Stmt k () -> Stmt k a -> Stmt k a
  (:.=) :: Ref k b -> Expr k b -> Stmt k a -> Stmt k a
  (:+=) :: Ref k b -> Expr k b -> Stmt k a -> Stmt k a
  (:*=) :: Ref k b -> Expr k b -> Stmt k a -> Stmt k a
  (:*!=) :: Ref k (v b) -> Expr k (v (v b)) -> Stmt k a -> Stmt k a
  Stmt :: Pretty b => b -> (b -> Stmt k a) -> Stmt k a

infixr 4 :.=
infixr 4 :+=
infixr 4 :*=
infixr 4 :*!=

instance Functor (Stmt k) where
  fmap = liftM

instance Applicative (Stmt k) where
  pure = Pure
  (<*>) = ap

instance Monad (Stmt k) where
  Pure a       >>= f = f a
  Let n v    k >>= f = Let n v (f <=< k)
  Discard    k >>= f = Discard (k >>= f)
  If c t e   k >>= f = If c t e (k >>= f)
  Switch s c k >>= f = Switch s c (k >>= f)
  Break      k >>= f = Break (k >>= f)
  While c t  k >>= f = While c t (k >>= f)
  (:.=) r v  k >>= f = (r :.= v) (k >>= f)
  (:+=) r v  k >>= f = (r :+= v) (k >>= f)
  (:*=) r v  k >>= f = (r :*= v) (k >>= f)
  (:*!=) r v k >>= f = (r :*!= v) (k >>= f)
  Stmt a     k >>= f = Stmt a (f <=< k)


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
  (:!*) :: Expr k (v (v Float)) -> Expr k (v Float) -> Expr k (v Float)
  (:!*!) :: Expr k (v (v Float)) -> Expr k (v (v Float)) -> Expr k (v (v Float))

  Eq :: Expr k a -> Expr k a -> Expr k Bool
  Lt :: Expr k a -> Expr k a -> Expr k Bool
  Gt :: Expr k a -> Expr k a -> Expr k Bool

  Get :: Ref k a -> Expr k a

  Float :: Expr k a -> Expr k Float
  Vec2 :: Expr k Float -> Expr k Float -> Expr k (V2 Float)
  Vec3 :: Expr k Float -> Expr k Float -> Expr k Float -> Expr k (V3 Float)
  Vec4 :: Expr k Float -> Expr k Float -> Expr k Float -> Expr k Float -> Expr k (V4 Float)
  Mat2 :: Expr k (V2 Float) -> Expr k (V2 Float) -> Expr k (M22 Float)
  Mat3 :: Expr k (V3 Float) -> Expr k (V3 Float) -> Expr k (V3 Float) -> Expr k (M33 Float)
  Mat4 :: Expr k (V4 Float) -> Expr k (V4 Float) -> Expr k (V4 Float) -> Expr k (V4 Float) -> Expr k (M44 Float)
  Ext3 :: Expr k (V2 Float) -> Expr k Float -> Expr k (V3 Float)
  Ext4 :: Expr k (V3 Float) -> Expr k Float -> Expr k (V4 Float)
  Norm :: Expr k (v Float) -> Expr k Float
  Dot :: Expr k (v Float) -> Expr k (v Float) -> Expr k (v Float)
  Lerp :: Expr k Float -> Expr k (v Float) -> Expr k (v Float) -> Expr k (v Float)
  Lerp2 :: Expr k (v Float) -> Expr k (v Float) -> Expr k (v Float) -> Expr k (v Float)
  Dfdx :: Expr k Float -> Expr k Float
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


data Ref (k :: Type) t
  = Ref String
  | forall s . Ref k s :^^. Prj s t

newtype Prj s t = Prj String


let' :: GL.Uniform a => String -> Expr k a -> Stmt k (Expr k a)
let' n v = Let n v (pure . Var . getConst)

var :: GL.Uniform a => String -> Expr k a -> Stmt k (Ref k a)
var n v = Let n v (pure . Ref . getConst)


get :: Ref k a -> Expr k a
get = Get


float :: Expr k a -> Expr k Float
float = Float

vec2 :: Expr k Float -> Expr k Float -> Expr k (V2 Float)
vec2 = Vec2

vec3 :: Expr k Float -> Expr k Float -> Expr k Float -> Expr k (V3 Float)
vec3 = Vec3

vec4 :: Expr k Float -> Expr k Float -> Expr k Float -> Expr k Float -> Expr k (V4 Float)
vec4 = Vec4

mat2 :: Expr k (V2 Float) -> Expr k (V2 Float) -> Expr k (M22 Float)
mat2 = Mat2

mat3 :: Expr k (V3 Float) -> Expr k (V3 Float) -> Expr k (V3 Float) -> Expr k (M33 Float)
mat3 = Mat3

mat4 :: Expr k (V4 Float) -> Expr k (V4 Float) -> Expr k (V4 Float) -> Expr k (V4 Float) -> Expr k (M44 Float)
mat4 = Mat4

ext3 :: Expr k (V2 Float) -> Expr k Float -> Expr k (V3 Float)
ext3 = Ext3

ext4 :: Expr k (V3 Float) -> Expr k Float -> Expr k (V4 Float)
ext4 = Ext4

norm :: Expr k (v Float) -> Expr k Float
norm = Norm

dot :: Expr k (v Float) -> Expr k (v Float) -> Expr k (v Float)
dot = Dot

lerp :: Expr k Float -> Expr k (v Float) -> Expr k (v Float) -> Expr k (v Float)
lerp = Lerp

lerp2 :: Expr k (v Float) -> Expr k (v Float) -> Expr k (v Float) -> Expr k (v Float)
lerp2 = Lerp2

dFdx :: Expr k Float -> Expr k Float
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


gl_Position :: Ref 'Vertex (V4 Float)
gl_Position = Ref "gl_Position"

gl_PointSize :: Ref 'Vertex Float
gl_PointSize = Ref "gl_PointSize"

gl_InstanceID :: Expr 'Vertex Int
gl_InstanceID = Var "gl_InstanceID"


gl_FragCoord :: Expr 'Fragment (V2 Float)
gl_FragCoord = Var "gl_FragCoord"

gl_FrontFacing :: Expr 'Fragment Bool
gl_FrontFacing = Var "gl_FrontFacing"

gl_PointCoord :: Expr 'Fragment (V2 Float)
gl_PointCoord = Var "gl_PointCoord"

discard :: Stmt 'Fragment ()
discard = Discard (pure ())


iff :: Expr k Bool -> Stmt k () -> Stmt k () -> Stmt k ()
iff c t e = If c t e (pure ())

switch :: Expr k Int -> [(Maybe Int, Stmt k ())] -> Stmt k ()
switch s cs = Switch s cs (pure ())

break :: Stmt k ()
break = Break (pure ())

while :: Expr k Bool -> Stmt k () -> Stmt k ()
while c t = While c t (pure ())

eq :: Expr k a -> Expr k a -> Expr k Bool
eq = Eq

infix 4 `eq`

lt :: Expr k a -> Expr k a -> Expr k Bool
lt = Lt

infix 4 `lt`

gt :: Expr k a -> Expr k a -> Expr k Bool
gt = Gt

infix 4 `gt`


(.=) :: Ref k a -> Expr k a -> Stmt k ()
r .= v = (r :.= v) (pure ())

infixr 4 .=

(+=) :: Ref k a -> Expr k a -> Stmt k ()
r += v = (r :+= v) (pure ())

infixr 4 +=

(*=) :: Ref k a -> Expr k a -> Stmt k ()
r *= v = (r :*= v) (pure ())

infixr 4 *=

(*!=) :: Ref k (v a) -> Expr k (v (v a)) -> Stmt k ()
r *!= v = (r :*!= v) (pure ())

infixr 4 *!=

(^.) :: Expr k a -> Prj a b -> Expr k b
(^.) = (:^.)

infixl 8 ^.

(^^.) :: Ref k a -> Prj a b -> Ref k b
(^^.) = (:^^.)

infixl 8 ^^.

_x :: Prj (v a) a
_x = Prj "x"

_y :: Prj (v a) a
_y = Prj "y"

_z :: Prj (v a) a
_z = Prj "z"

_w :: Prj (v a) a
_w = Prj "w"

_xy :: Prj (v a) (V2 a)
_xy = Prj "xy"

_yz :: Prj (v a) (V2 a)
_yz = Prj "yz"

_xz :: Prj (v a) (V2 a)
_xz = Prj "xz"

_xw :: Prj (v a) (V2 a)
_xw = Prj "xw"

_zw :: Prj (v a) (V2 a)
_zw = Prj "zw"

_xyz :: Prj (v a) (V3 a)
_xyz = Prj "xyz"

_xywz :: Prj (v a) (V4 a)
_xywz = Prj "xywz"

_a :: Prj (v a) a
_a = Prj "a"


(^*) :: Expr k (v a) -> Expr k a -> Expr k (v a)
(^*) = (:^*)

infixl 7 ^*


(!*) :: Expr k (v (v Float)) -> Expr k (v Float) -> Expr k (v Float)
(!*) = (:!*)

infixl 7 !*

(!*!) :: Expr k (v (v Float)) -> Expr k (v (v Float)) -> Expr k (v (v Float))
(!*!) = (:!*!)

infixl 7 !*!


renderStmt :: Stmt k a -> Doc ()
renderStmt = \case
  Pure _ -> mempty
  Let n v k
    -> renderTypeOf v <+> pretty n <+> pretty '=' <+> renderExpr v <> pretty ';' <> hardline
    <> renderStmt (k (Const n))
  Discard k
    -> pretty "discard" <> pretty ';' <> hardline
    <> renderStmt k
  If c t e k
    -> pretty "if" <+> parens (renderExpr c) <+> braces (nest 2 (line <> renderStmt t <> line)) <+> pretty "else" <+> braces (nest 2 (line <> renderStmt e <> line)) <> hardline
    <> renderStmt k
  Switch s cs k
    -> pretty "switch" <+> parens (renderExpr s) <+> braces (nest 2 (line <> vsep (map renderCase cs) <> line)) <> hardline
    <> renderStmt k
  Break k
    -> pretty "break" <+> pretty ';' <> hardline
    <> renderStmt k
  While c t k
    -> pretty "while" <+> parens (renderExpr c) <+> braces (nest 2 (line <> renderStmt t <> line)) <> hardline
    <> renderStmt k
  (:.=) r v k
    -> renderRef r <+> pretty '=' <+> renderExpr v <> pretty ';' <> hardline
    <> renderStmt k
  (:+=) r v k
    -> renderRef r <+> pretty "+=" <+> renderExpr v <> pretty ';' <> hardline
    <> renderStmt k
  (:*=) r v k
    -> renderRef r <+> pretty "*=" <+> renderExpr v <> pretty ';' <> hardline
    <> renderStmt k
  (:*!=) r v k
    -> renderRef r <+> pretty "*=" <+> renderExpr v <> pretty ';' <> hardline
    <> renderStmt k
  Stmt b k
    -> pretty b <> pretty ';' <> hardline
    <> renderStmt (k b)
  where
  renderCase (i, s) = maybe (pretty "default:" <> hardline) (\ i -> pretty "case" <+> pretty i <> pretty ':') i  <> hardline <> renderStmt s

renderExpr :: Expr k a -> Doc ()
renderExpr = parens . \case
  Var n -> pretty n
  Lit d -> pretty d
  a :+ b -> renderExpr a <+> pretty '+' <+> renderExpr b
  a :* b -> renderExpr a <+> pretty '*' <+> renderExpr b
  a :- b -> renderExpr a <+> pretty '-' <+> renderExpr b
  a :/ b -> renderExpr a <+> pretty '/' <+> renderExpr b
  Signum a -> fn "signum" [renderExpr a] -- log
  Negate a -> pretty "-" <> renderExpr a
  Abs a -> fn "abs" [renderExpr a]
  FromInteger i -> pretty i
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
  a :^. Prj s -> renderExpr a <> pretty '.' <> pretty s
  a :^* b -> renderExpr a <+> pretty '*' <+> renderExpr b
  a :!* b -> renderExpr a <+> pretty '*' <+> renderExpr b
  a :!*! b -> renderExpr a <+> pretty '*' <+> renderExpr b
  Eq a b -> renderExpr a <+> pretty "==" <+> renderExpr b
  Lt a b -> renderExpr a <+> pretty '<' <+> renderExpr b
  Gt a b -> renderExpr a <+> pretty '>' <+> renderExpr b
  Get r -> renderRef r
  Float a -> fn "float" [renderExpr a]
  Vec2 a b -> fn "vec2" [renderExpr a, renderExpr b]
  Vec3 a b c -> fn "vec3" [renderExpr a, renderExpr b, renderExpr c]
  Vec4 a b c d -> fn "vec4" [renderExpr a, renderExpr b, renderExpr c, renderExpr d]
  Mat2 a b -> fn "mat2" [renderExpr a, renderExpr b]
  Mat3 a b c -> fn "mat3" [renderExpr a, renderExpr b, renderExpr c]
  Mat4 a b c d -> fn "mat4" [renderExpr a, renderExpr b, renderExpr c, renderExpr d]
  Ext3 a b -> fn "vec3" [renderExpr a, renderExpr b]
  Ext4 a b -> fn "vec4" [renderExpr a, renderExpr b]
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

renderRef :: Ref k a -> Doc ()
renderRef = \case
  Ref n        -> pretty n
  r :^^. Prj p -> renderRef r <> pretty '.' <> pretty p
