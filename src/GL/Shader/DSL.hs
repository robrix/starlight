{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module GL.Shader.DSL
( Shader(..)
, None(..)
, shaderSources
, Stmt
, Expr
, Ref
, Prj
, let'
, var
, get
, vec2
, vec3
, vec4
, norm
, dot
, lerp
, lerp2
, dFdx
, mod'
, min'
, max'
, texture
, coerce
, gl_Position
, gl_PointSize
, gl_FragCoord
, gl_FrontFacing
, gl_PointCoord
, discard
, iff
, while
, eq
, lt
, gt
, (.=)
, (+=)
, (*=)
, (^.)
, _x
, _y
, _z
, _w
, _xy
, _yz
, _zw
, _xyz
, _xywz
, _a
, (^*)
, (!*)
, renderStmt
, renderExpr
, GLSLType(..)
, Vars(..)
  -- * Re-exports
, Type(..)
, Colour
, M33
, Point(..)
, TextureUnit
, V2
, V3
, V4
) where

import           Control.Monad (ap, liftM, (<=<))
import qualified Data.Coerce as C
import           Data.Function (fix)
import           Data.Functor.Const
import           Data.Proxy
import           Data.Text.Prettyprint.Doc hiding (dot)
import           Data.Text.Prettyprint.Doc.Render.String
import           GHC.Generics
import           GL.Shader (Type(..))
import           GL.TextureUnit
import qualified GL.Uniform as GL
import           Linear.Affine (Point(..))
import           Linear.Matrix (M33)
import           Linear.V2 (V2(..))
import           Linear.V3 (V3(..))
import           Linear.V4 (V4(..))
import           UI.Colour (Colour)
import           Unit.Angle

data Shader (u :: (* -> *) -> *) (i :: (* -> *) -> *) (o :: (* -> *) -> *) where
  V :: (Vars u, Vars i, Vars o) => (u (Expr k) -> i (Expr k) -> o (Ref k) -> Stmt k ()) -> Shader u o o' -> Shader u i o'
  F :: (Vars u, Vars i, Vars o) => (u (Expr k) -> i (Expr k) -> o (Ref k) -> Stmt k ()) -> Shader u o o' -> Shader u i o'
  Nil :: Shader u i o

data None (v :: * -> *) = None
  deriving (Generic, Vars)

shaderSources :: Shader u i o -> [(Type, String)]
shaderSources = \case
  V s k -> (Vertex,   renderString (layoutPretty defaultLayoutOptions (renderShader s))) : shaderSources k
  F s k -> (Fragment, renderString (layoutPretty defaultLayoutOptions (renderShader s))) : shaderSources k
  Nil -> []
  where
  renderShader f
    =  pretty "#version 410" <> hardline
    <> foldVars (const getConst) (makeVars (var "uniform") `like` u)
    <> foldVars (const getConst) (makeVars (var "in") `like` i)
    <> foldVars (const getConst) (makeVars (var "out") `like` o)
    <> pretty "void" <+> pretty "main" <> parens mempty <+> braces (nest 2 (line <> renderStmt (f u i o) <> line)) where
    u = makeVars Var
    i = makeVars Var
    o = makeVars Ref

    like :: t (Const a) -> t b -> t (Const a)
    like = const

    var :: GLSLType a => String -> String -> Const (Doc ()) a
    var qual n = fix $ \ c -> Const $ pretty qual <+> renderTypeOf c <+> pretty n <> pretty ';' <> hardline


data Stmt (k :: Type) a where
  Pure :: a -> Stmt k a
  Let :: GLSLType b => String -> Expr k b -> (Const String b -> Stmt k a) -> Stmt k a
  Discard :: Stmt 'Fragment a -> Stmt 'Fragment a
  If :: Expr k Bool -> Stmt k () -> Stmt k () -> Stmt k a -> Stmt k a
  While :: Expr k Bool -> Stmt k () -> Stmt k a -> Stmt k a
  (:.=) :: Ref k b -> Expr k b -> Stmt k a -> Stmt k a
  (:+=) :: Ref k b -> Expr k b -> Stmt k a -> Stmt k a
  (:*=) :: Ref k b -> Expr k b -> Stmt k a -> Stmt k a
  Stmt :: Pretty b => b -> (b -> Stmt k a) -> Stmt k a

infixr 4 :.=
infixr 4 :+=
infixr 4 :*=

instance Functor (Stmt k) where
  fmap = liftM

instance Applicative (Stmt k) where
  pure = Pure
  (<*>) = ap

instance Monad (Stmt k) where
  Pure a      >>= f = f a
  Let n v   k >>= f = Let n v (f <=< k)
  Discard   k >>= f = Discard (k >>= f)
  If c t e  k >>= f = If c t e (k >>= f)
  While c t k >>= f = While c t (k >>= f)
  (:.=) r v k >>= f = (r :.= v) (k >>= f)
  (:+=) r v k >>= f = (r :+= v) (k >>= f)
  (:*=) r v k >>= f = (r :*= v) (k >>= f)
  Stmt a    k >>= f = Stmt a (f <=< k)


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
  (:!*) :: Expr k (M33 Float) -> Expr k (V3 Float) -> Expr k (V3 Float)

  Eq :: Expr k a -> Expr k a -> Expr k Bool
  Lt :: Expr k a -> Expr k a -> Expr k Bool
  Gt :: Expr k a -> Expr k a -> Expr k Bool

  Get :: Ref k a -> Expr k a

  Vec2 :: Expr k Float -> Expr k Float -> Expr k (V2 Float)
  Vec3 :: Expr k (V2 Float) -> Expr k Float -> Expr k (V3 Float)
  Vec4 :: Expr k (V3 Float) -> Expr k Float -> Expr k (V4 Float)
  Norm :: Expr k (v Float) -> Expr k Float
  Dot :: Expr k (v Float) -> Expr k (v Float) -> Expr k (v Float)
  Lerp :: Expr k Float -> Expr k (v Float) -> Expr k (v Float) -> Expr k (v Float)
  Lerp2 :: Expr k (v Float) -> Expr k (v Float) -> Expr k (v Float) -> Expr k (v Float)
  Dfdx :: Expr k Float -> Expr k Float
  Mod :: Expr k (v Float) -> Expr k (v Float) -> Expr k (v Float)
  Min :: Expr k a -> Expr k a -> Expr k a
  Max :: Expr k a -> Expr k a -> Expr k a
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


newtype Ref (k :: Type) t = Ref String

newtype Prj s t = Prj String


let' :: GLSLType a => String -> Expr k a -> Stmt k (Expr k a)
let' n v = Let n v (pure . Var . getConst)

var :: GLSLType a => String -> Expr k a -> Stmt k (Ref k a)
var n v = Let n v (pure . Ref . getConst)


get :: Ref k a -> Expr k a
get = Get


vec2 :: Expr k Float -> Expr k Float -> Expr k (V2 Float)
vec2 = Vec2

vec3 :: Expr k (V2 Float) -> Expr k Float -> Expr k (V3 Float)
vec3 = Vec3

vec4 :: Expr k (V3 Float) -> Expr k Float -> Expr k (V4 Float)
vec4 = Vec4

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

mod' :: Expr k (v Float) -> Expr k (v Float) -> Expr k (v Float)
mod' = Mod

min' :: Expr k a -> Expr k a -> Expr k a
min' = Min

max' :: Expr k a -> Expr k a -> Expr k a
max' = Max

texture :: Expr k TextureUnit -> Expr k (v Float) -> Expr k (v Float)
texture = Texture


coerce :: C.Coercible a b => Expr k a -> Expr k b
coerce = Coerce


gl_Position :: Ref 'Vertex (V4 Float)
gl_Position = Ref "gl_Position"

gl_PointSize :: Ref 'Vertex Float
gl_PointSize = Ref "gl_PointSize"


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

(^.) :: Expr k a -> Prj a b -> Expr k b
(^.) = (:^.)

infixl 8 ^.

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


(!*) :: Expr k (M33 Float) -> Expr k (V3 Float) -> Expr k (V3 Float)
(!*) = (:!*)

infixl 7 !*


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
  While c t k
    -> pretty "while" <+> parens (renderExpr c) <+> braces (nest 2 (line <> renderStmt t <> line)) <> hardline
    <> renderStmt k
  (:.=) (Ref r) v k
    -> pretty r <+> pretty '=' <+> renderExpr v <> pretty ';' <> hardline
    <> renderStmt k
  (:+=) (Ref r) v k
    -> pretty r <+> pretty "+=" <+> renderExpr v <> pretty ';' <> hardline
    <> renderStmt k
  (:*=) (Ref r) v k
    -> pretty r <+> pretty "*=" <+> renderExpr v <> pretty ';' <> hardline
    <> renderStmt k
  Stmt b k
    -> pretty b <> pretty ';' <> hardline
    <> renderStmt (k b)

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
  Eq a b -> renderExpr a <+> pretty "==" <+> renderExpr b
  Lt a b -> renderExpr a <+> pretty '<' <+> renderExpr b
  Gt a b -> renderExpr a <+> pretty '>' <+> renderExpr b
  Get (Ref n) -> pretty n
  Vec2 a b -> fn "vec2" [renderExpr a, renderExpr b]
  Vec3 a b -> fn "vec3" [renderExpr a, renderExpr b]
  Vec4 a b -> fn "vec4" [renderExpr a, renderExpr b]
  Norm a -> fn "length" [renderExpr a]
  Dot a b -> fn "dot" [renderExpr a, renderExpr b]
  Lerp t a b -> fn "mix" [renderExpr a, renderExpr b, renderExpr t]
  Lerp2 t a b -> fn "mix" [renderExpr a, renderExpr b, renderExpr t]
  Dfdx a -> fn "dFdx" [renderExpr a]
  Mod a b -> fn "mod" [renderExpr a, renderExpr b]
  Min a b -> fn "min" [renderExpr a, renderExpr b]
  Max a b -> fn "max" [renderExpr a, renderExpr b]
  Texture a b -> fn "texture" [renderExpr a, renderExpr b]
  Coerce a -> renderExpr a
  where
  fn n as = pretty n <> tupled as


class GL.Uniform a => GLSLType a where
  renderTypeOf :: expr a -> Doc ()

instance GLSLType a => GLSLType (Radians a) where
  renderTypeOf _ = renderTypeOf (Proxy @a)

instance GLSLType (f a) => GLSLType (Point f a) where
  renderTypeOf _ = renderTypeOf (Proxy @(f a))

instance GLSLType Int where
  renderTypeOf _ = pretty "int"

instance GLSLType Float where
  renderTypeOf _ = pretty "float"

instance GLSLType (V2 Float) where
  renderTypeOf _ = pretty "vec2"

instance GLSLType (V3 Float) where
  renderTypeOf _ = pretty "vec3"

instance GLSLType (V3 (V3 Float)) where
  renderTypeOf _ = pretty "mat3"

instance GLSLType (V4 Float) where
  renderTypeOf _ = pretty "vec4"

instance GLSLType TextureUnit where
  renderTypeOf _ = pretty "sampler2D"


class Vars t where
  makeVars :: (forall a . GLSLType a => String -> v a) -> t v
  default makeVars :: (Generic (t v), GVars v t (Rep (t v))) => (forall a . GLSLType a => String -> v a) -> t v
  makeVars f = to (gmakeVars @_ @t f)

  foldVars :: Monoid b => (forall a . GLSLType a => String -> v a -> b) -> t v -> b
  default foldVars :: (Generic (t v), GVars v t (Rep (t v)), Monoid b) => (forall a . GLSLType a => String -> v a -> b) -> t v -> b
  foldVars f = gfoldVars @_ @t f . from


class GVars v t f where
  gmakeVars :: (forall a . GLSLType a => String -> v a) -> f (t v)

  gfoldVars :: Monoid b => (forall a . GLSLType a => String -> v a -> b) -> f (t v) -> b

instance GVars v t f => GVars v t (M1 D d f) where
  gmakeVars f = M1 $ gmakeVars f

  gfoldVars f = gfoldVars f . unM1

instance GVars v t f => GVars v t (M1 C c f) where
  gmakeVars f = M1 $ gmakeVars f

  gfoldVars f = gfoldVars f . unM1

instance GVars v t U1 where
  gmakeVars _ = U1

  gfoldVars _ _ = mempty

instance (GVars v t f, GVars v t g) => GVars v t (f :*: g) where
  gmakeVars f = gmakeVars f :*: gmakeVars f

  gfoldVars f (l :*: r) = gfoldVars f l <> gfoldVars f r

instance (GVar v t f, Selector s) => GVars v t (M1 S s f) where
  gmakeVars f = fix $ \ x -> M1 (gmakeVar f (selName x))

  gfoldVars f m = gfoldVar f (selName m) (unM1 m)

class GVar v t f where
  gmakeVar :: (forall a . GLSLType a => String -> v a) -> String -> f (t v)

  gfoldVar :: Monoid b => (forall a . GLSLType a => String -> v a -> b) -> String -> f (t v) -> b

instance GLSLType a => GVar v t (K1 R (v a)) where
  gmakeVar f s = K1 (f s)

  gfoldVar f s = f s . unK1
