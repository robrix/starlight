{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# HLINT ignore "Use camelCase" #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
module GL.Shader.DSL
( Shader(..)
, RShader(..)
, (Cat.>>>)
, None(..)
, shaderSources
, Frag(..)
  -- * Decls
, RDecl
, Decl(..)
, VertexDecl
, GeometryDecl(..)
, FragmentDecl
  -- * Stmts
, RStmt
, Stmt(..)
, VertexStmt
, GeometryStmt(..)
, FragmentStmt(..)
  -- * References (mutable variables)
, RRef(..)
, Ref(..)
, VertexRef(..)
, GeometryRef
, FragmentRef

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

  -- * Expressions
, RExpr(..)
, Vec(..)
, xy
, xyz
, xyzw
, rgba
, Mat(..)
, Expr(..)
, cast
, float
, VertexExpr(..)
, GeometryExpr
, FragmentExpr(..)
  -- * Re-exports
, Fields(..)
, Vars
, Colour
, M22
, M33
, M44
, TextureUnit
, V2(..)
, V3(..)
, V4(..)
) where

import qualified Control.Category as Cat
import           Control.Monad.Trans.Cont
import           Data.Coerce
import           Data.Function (fix)
import           Data.Functor.C
import           Data.Functor.K
import           Data.Kind (Type)
import           Data.Text.Prettyprint.Doc hiding (dot)
import           Data.Text.Prettyprint.Doc.Render.String
import           GHC.Generics hiding ((:.:))
import qualified GL.Primitive as P
import qualified GL.Shader as Shader
import           GL.Shader.Vars
import           GL.TextureUnit
import qualified GL.Uniform as GL
import           Linear.Matrix (M22, M33, M44)
import           Linear.V2 (V2(..))
import           Linear.V3 (V3(..))
import           Linear.V4 (V4(..))
import           Prelude hiding (break)
import           UI.Colour (Colour)

class (forall u . Cat.Category (shader u)) => Shader shader where
  vertex :: (Vars u, Vars i, Vars o) => (forall ref expr stmt decl . VertexDecl ref expr stmt decl => u expr -> i expr -> o ref -> decl ()) -> shader u i o
  geometry :: (Vars u, Vars i, Vars o) => (forall ref expr stmt decl . GeometryDecl ref expr stmt decl => u expr -> i (expr :.: []) -> o ref -> decl ()) -> shader u i o
  fragment :: (Vars u, Vars i, Vars o) => (forall ref expr stmt decl . FragmentDecl ref expr stmt decl => u expr -> i expr -> o ref -> decl ()) -> shader u i o

newtype RShader (u :: (Type -> Type) -> Type) (i :: (Type -> Type) -> Type) (o :: (Type -> Type) -> Type) = RShader { renderShader :: [(Shader.Stage, Doc ())] }

instance Cat.Category (RShader u) where
  id = RShader []
  RShader l . RShader r = RShader $ r ++ l

instance Shader RShader where
  vertex   s = RShader $ renderStage Shader.Vertex   id (makeVars (RExpr . pretty . name)) s
  geometry s = RShader $ renderStage Shader.Geometry (coerce :: forall x . RExpr x -> (RExpr :.: []) x) (makeVars (RExpr . pretty . name)) s
  fragment s = RShader $ renderStage Shader.Fragment id (makeVars (RExpr . pretty . name)) s


data None (v :: Type -> Type) = None
  deriving (Generic)

instance Vars None


shaderSources :: RShader u i o -> [(Shader.Stage, String)]
shaderSources (RShader f) = fmap (renderString . layoutPretty defaultLayoutOptions) <$> f

renderStage :: (Vars u, Vars i, Vars o) => Shader.Stage -> (forall a . RExpr a -> f a) -> u RExpr -> (u RExpr -> i f -> o RRef -> RDecl ()) -> [(Shader.Stage, Doc ())]
renderStage t g u f = pure . (,) t
  $  pretty "#version 410" <> hardline
  <> foldVars (getK . value) (makeVars (pvar "uniform" . name) `like` u)
  <> case t of
    Shader.Geometry -> foldVars (getK . value) (makeVars (pvar "in" . (<> "[]") . name) `like` i)
    _               -> foldVars (getK . value) (makeVars (pvar "in"      . name) `like` i)
  <> foldVars (getK . value) (makeVars (pvar "out"     . name) `like` o)
  <> renderDecl (f u i o) where
  i = makeVars (g . RExpr . pretty . name)
  o = makeVars (RRef . pretty . name)

like :: t (K a) -> t b -> t (K a)
like = const

pvar :: GL.Uniform a => String -> String -> K (Doc ()) a
pvar qual n = fix $ \ c -> K $ pretty qual <+> renderTypeOf c <+> pretty n <> pretty ';' <> hardline


newtype Frag v = Frag { fragColour :: v (Colour Float) }
  deriving (Generic)

instance Vars Frag


-- Decls

renderDecl :: RDecl a -> Doc ()
renderDecl = (`runCont` const mempty) . getDecl

newtype RDecl a = RDecl { getDecl :: Cont (Doc ()) a }
  deriving (Applicative, Functor, Monad)

decl :: Doc () -> RDecl ()
decl d = RDecl . cont $ \ k -> d <> k ()

class (Stmt ref expr stmt, Monad decl) => Decl ref expr stmt decl | decl -> ref expr stmt where
  main :: stmt () -> decl ()

class (VertexStmt ref expr stmt, Decl ref expr stmt decl) => VertexDecl ref expr stmt decl where

class (GeometryStmt ref expr stmt, Decl ref expr stmt decl) => GeometryDecl ref expr stmt decl where
  primitiveIn :: P.Type -> decl ()
  primitiveOut :: P.Type -> Int -> decl ()

class (FragmentStmt ref expr stmt, Decl ref expr stmt decl) => FragmentDecl ref expr stmt decl where

instance Decl RRef RExpr RStmt RDecl where
  main body = decl (pretty "void" <+> pretty "main" <> parens mempty <+> braces (nest 2 (line <> renderStmt body <> line)))

instance VertexDecl RRef RExpr RStmt RDecl where

instance GeometryDecl RRef RExpr RStmt RDecl where
  primitiveIn ty = decl doc where
    doc = pretty "layout" <+> parens (render ty) <+> pretty "in;" <> hardline
    render = \case
      P.Points        -> pretty "points"
      P.Lines         -> pretty "lines"
      P.LineStrip     -> pretty "lines"
      P.LineLoop      -> pretty "lines"
      P.TriangleStrip -> pretty "triangles"
      P.Triangles     -> pretty "triangles"

  primitiveOut ty mx = decl doc where
    doc = pretty "layout" <+> parens (render ty <> comma <+> pretty "max_vertices" <+> equals <+> pretty mx) <+> pretty "out;" <> hardline
    render = \case
      P.Points        -> pretty "points"
      P.Lines         -> pretty "line_strip"
      P.LineStrip     -> pretty "line_strip"
      P.LineLoop      -> pretty "line_strip"
      P.TriangleStrip -> pretty "triangle_strip"
      P.Triangles     -> pretty "triangle_strip"

instance FragmentDecl RRef RExpr RStmt RDecl where


-- Stmts

renderStmt :: RStmt () -> Doc ()
renderStmt = (`runCont` const mempty) . getStmt

newtype RStmt a = RStmt { getStmt :: Cont (Doc ()) a }
  deriving (Applicative, Functor, Monad)

class (Expr ref expr, Monad stmt) => Stmt ref expr stmt | stmt -> ref expr where
  let' :: GL.Uniform a => String -> expr a -> stmt (expr a)
  var :: GL.Uniform a => String -> expr a -> stmt (ref a)

  iff :: expr Bool -> stmt () -> stmt () -> stmt ()
  switch :: expr Int -> [(Maybe Int, stmt ())] -> stmt ()
  break :: stmt ()
  while :: expr Bool -> stmt () -> stmt ()

  (.=) :: ref a -> expr a -> stmt ()
  (+=) :: ref a -> expr a -> stmt ()
  (*=) :: ref a -> expr a -> stmt ()
  (*!=) :: ref (v a) -> expr (v (v a)) -> stmt ()

  infixr 4 .=, +=, *=, *!=

class (VertexExpr ref expr, Stmt ref expr stmt) => VertexStmt ref expr stmt where

class (GeometryExpr ref expr, Stmt ref expr stmt) => GeometryStmt ref expr stmt where
  emitVertex :: stmt () -> stmt ()
  emitPrimitive :: stmt () -> stmt ()

class (FragmentExpr ref expr, Stmt ref expr stmt) => FragmentStmt ref expr stmt where
  discard :: stmt ()

instance Stmt RRef RExpr RStmt where
  let' n v = RStmt . cont $ \ k
    -> renderTypeOf v <+> pretty n <+> pretty '=' <+> renderExpr v <> pretty ';' <> hardline
    <> k (RExpr (pretty n))

  var n v = RStmt . cont $ \ k
    -> renderTypeOf v <+> pretty n <+> pretty '=' <+> renderExpr v <> pretty ';' <> hardline
    <> k (RRef (pretty n))

  iff c t e = stmt $ pretty "if" <+> parens (renderExpr c) <+> braces (nest 2 (line <> renderStmt t <> line)) <+> pretty "else" <+> braces (nest 2 (line <> renderStmt e <> line)) <> hardline

  switch s cs = stmt $ pretty "switch" <+> parens (renderExpr s) <+> braces (nest 2 (line <> vsep (map renderCase cs) <> line)) <> hardline
    where
    renderCase (i, s) = maybe (pretty "default:" <> hardline) (\ i -> pretty "case" <+> pretty i <> pretty ':') i  <> hardline <> renderStmt s

  break = stmt $ pretty "break" <+> pretty ';' <> hardline

  while c t = stmt $ pretty "while" <+> parens (renderExpr c) <+> braces (nest 2 (line <> renderStmt t <> line)) <> hardline

  r .= v = stmt $ renderRef r <+> pretty '=' <+> renderExpr v <> pretty ';' <> hardline

  r += v = stmt $ renderRef r <+> pretty "+=" <+> renderExpr v <> pretty ';' <> hardline

  r *= v = stmt $ renderRef r <+> pretty "*=" <+> renderExpr v <> pretty ';' <> hardline

  r *!= v = stmt $ renderRef r <+> pretty "*=" <+> renderExpr v <> pretty ';' <> hardline

instance VertexStmt RRef RExpr RStmt where

instance GeometryStmt RRef RExpr RStmt where
  emitVertex m = stmt
    $  renderStmt m <> hardline
    <> pretty "EmitVertex();" <> hardline

  emitPrimitive m = stmt
    $  renderStmt m <> hardline
    <> pretty "EndPrimitive();" <> hardline

instance FragmentStmt RRef RExpr RStmt where
  discard = stmt $ pretty "discard" <> pretty ';' <> hardline

renderTypeOf :: forall a expr . GL.Uniform a => expr a -> Doc ()
renderTypeOf _ = pretty (GL.glslType @a)


stmt :: Doc () -> RStmt ()
stmt d = RStmt . cont $ \ k -> d <> k ()


-- Exprs

newtype RRef a = RRef { renderRef :: Doc () }

class Ref ref where
  gl_Position :: ref (V4 Float)

  (^^.) :: ref a -> Prj a b -> ref b
  infixl 8 ^^.

class Ref ref => VertexRef ref where
  gl_PointSize :: ref Float

class Ref ref => GeometryRef ref where

class Ref ref => FragmentRef ref where

instance Ref RRef where
  gl_Position = RRef $ pretty "gl_Position"

  r ^^. Prj p = RRef $ renderRef r <> pretty p

instance VertexRef RRef where
  gl_PointSize = RRef $ pretty "gl_PointSize"

instance GeometryRef RRef where

instance FragmentRef RRef where


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


class Vec expr where
  v2 :: GL.Row a => V2 (expr a) -> expr (V2 a)
  v3 :: GL.Row a => V3 (expr a) -> expr (V3 a)
  v4 :: GL.Row a => V4 (expr a) -> expr (V4 a)

  ext3 :: expr (V2 Float) -> expr Float -> expr (V3 Float)
  ext4 :: expr (V3 Float) -> expr Float -> expr (V4 Float)

  dext3 :: expr (V2 Double) -> expr Double -> expr (V3 Double)
  dext4 :: expr (V3 Double) -> expr Double -> expr (V4 Double)

  norm :: expr (v a) -> expr a
  dot :: expr (v a) -> expr (v a) -> expr a

  (^*) :: expr (v a) -> expr a -> expr (v a)
  (^/) :: expr (v a) -> expr a -> expr (v a)

  infixl 7 ^*, ^/

xy :: (Vec expr, GL.Row a) => expr a -> expr a -> expr (V2 a)
xy x y = v2 (V2 x y)

xyz :: (Vec expr, GL.Row a) => expr a -> expr a -> expr a -> expr (V3 a)
xyz x y z = v3 (V3 x y z)

xyzw :: (Vec expr, GL.Row a) => expr a -> expr a -> expr a -> expr a -> expr (V4 a)
xyzw x y z w = v4 (V4 x y z w)

rgba :: (Vec expr, GL.Row a) => expr a -> expr a -> expr a -> expr a -> expr (V4 a)
rgba r g b a = v4 (V4 r g b a)

class Vec expr => Mat expr where
  m2 :: GL.Column a => V2 (expr (V2 a)) -> expr (M22 a)
  m3 :: GL.Column a => V3 (expr (V3 a)) -> expr (M33 a)
  m4 :: GL.Column a => V4 (expr (V4 a)) -> expr (M44 a)

  (!*) :: expr (v (v a)) -> expr (v a) -> expr (v a)
  (!!*) :: expr (v (v a)) -> expr a -> expr (v (v a))
  (!*!) :: expr (v (v a)) -> expr (v (v a)) -> expr (v (v a))
  infixl 7 !*, !!*, !*!

class ( Ref ref
      , forall a . Num a => Num (expr a)
      , forall a . Fractional a => Fractional (expr a)
      , forall a . Floating a => Floating (expr a)
      , forall a b . Coercible a b => Coercible (expr a) (expr b)
      , Mat expr
      )
   => Expr ref expr | expr -> ref where
  get :: ref a -> expr a

  cast' :: GL.Uniform b => K (expr a) b -> expr b

  log2 :: expr a -> expr a
  exp2 :: expr a -> expr a

  lerp :: expr a -> expr (v a) -> expr (v a) -> expr (v a)
  lerp2 :: expr (v a) -> expr (v a) -> expr (v a) -> expr (v a)

  mod' :: expr v -> expr v -> expr v

  min' :: expr a -> expr a -> expr a
  max' :: expr a -> expr a -> expr a

  atan2' :: expr a -> expr a -> expr a

  texture :: expr TextureUnit -> expr (v Float) -> expr (v Float)

  fract :: expr a -> expr a

  eq :: expr a -> expr a -> expr Bool
  lt :: expr a -> expr a -> expr Bool
  gt :: expr a -> expr a -> expr Bool
  infix 4 `eq`, `lt`, `gt`

  (^.) :: expr a -> Prj a b -> expr b

  (!) :: (expr :.: []) a -> expr Int -> expr a
  infixl 9 !

cast :: forall a b expr ref . (Expr ref expr, GL.Uniform b) => expr a -> expr b
cast = cast' . K

float :: Expr ref expr => expr a -> expr Float
float = cast @_ @Float

class (VertexRef ref, Expr ref expr) => VertexExpr ref expr where
  gl_InstanceID :: expr Int

class (GeometryRef ref, Expr ref expr) => GeometryExpr ref expr where

class (FragmentRef ref, Expr ref expr) => FragmentExpr ref expr where
  gl_FragCoord :: expr (V2 Float)
  gl_FrontFacing :: expr Bool
  gl_PointCoord :: expr (V2 Float)

  dFdx :: expr a -> expr a
  dFdy :: expr a -> expr a

newtype RExpr a = RExpr { renderExpr :: Doc () }

instance Num (RExpr a) where
  a + b = RExpr . parens $ renderExpr a <+> pretty '+' <+> renderExpr b
  a * b = RExpr . parens $ renderExpr a <+> pretty '*' <+> renderExpr b
  a - b = RExpr . parens $ renderExpr a <+> pretty '-' <+> renderExpr b
  signum = fn "sign"
  negate a = RExpr . parens $ pretty "-" <> renderExpr a
  abs = fn "abs"
  fromInteger i = RExpr $ pretty i

instance Fractional (RExpr a) where
  a / b = RExpr . parens $ renderExpr a <+> pretty '/' <+> renderExpr b
  fromRational = lit . fromRational

instance Floating (RExpr a) where
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

instance Vec RExpr where
  v2 (V2 x y :: V2 (expr a)) = fn (GL.glslType @(V2 a)) x y
  v3 (V3 x y z :: V3 (expr a)) = fn (GL.glslType @(V3 a)) x y z
  v4 (V4 x y z w :: V4 (expr a)) = fn (GL.glslType @(V4 a)) x y z w

  ext3 = fn "vec3"
  ext4 = fn "vec4"

  dext3 = fn "dvec3"
  dext4 = fn "dvec4"

  norm = fn "length"
  dot = fn "dot"
  a ^*  b = RExpr . parens $ renderExpr a <+> pretty '*' <+> renderExpr b
  a ^/  b = RExpr . parens $ renderExpr a <+> pretty '/' <+> renderExpr b

instance Mat RExpr where
  m2 (V2 x y :: V2 (expr (V2 a))) = fn (GL.glslType @(V2 (V2 a))) x y
  m3 (V3 x y z :: V3 (expr (V3 a))) = fn (GL.glslType @(V3 (V3 a))) x y z
  m4 (V4 x y z w :: V4 (expr (V4 a))) = fn (GL.glslType @(V4 (V4 a))) x y z w

  a !*  b = RExpr . parens $ renderExpr a <+> pretty '*' <+> renderExpr b
  a !!*  b = RExpr . parens $ renderExpr a <+> pretty '*' <+> renderExpr b
  a !*! b = RExpr . parens $ renderExpr a <+> pretty '*' <+> renderExpr b

instance Expr RRef RExpr where
  get = RExpr . renderRef

  a ^. Prj s = RExpr $ renderExpr a <> pretty s
  eq a b = RExpr . parens $ renderExpr a <+> pretty "==" <+> renderExpr b
  lt a b = RExpr . parens $ renderExpr a <+> pretty '<' <+> renderExpr b
  gt a b = RExpr . parens $ renderExpr a <+> pretty '>' <+> renderExpr b

  cast' (K a :: K (expr a) b) = fn (GL.glslType @b) a

  log2 = fn "log2"
  exp2 = fn "exp2"
  fract = fn "fract"

  lerp  t a b = fn "mix" a b t
  lerp2 t a b = fn "mix" a b t

  mod' = fn "mod"
  min' = fn "min"
  max' = fn "max"
  atan2' = fn "atan"

  texture = fn "texture"

  (!) (C v) n = RExpr $ renderExpr v <> brackets (renderExpr n)

instance VertexExpr RRef RExpr where
  gl_InstanceID = RExpr $ pretty "gl_InstanceID"

instance GeometryExpr RRef RExpr where

instance FragmentExpr RRef RExpr where
  gl_FragCoord = RExpr $ pretty "gl_FragCoord"
  gl_FrontFacing = RExpr $ pretty "gl_FrontFacing"
  gl_PointCoord = RExpr $ pretty "gl_PointCoord"

  dFdx = fn "dFdx"
  dFdy = fn "dFdy"

fn :: Fn t => String -> t
fn n = fn' n id

class Fn t where
  fn' :: String -> ([Doc ()] -> [Doc ()]) -> t

instance Fn (RExpr a) where
  fn' n as = RExpr $ pretty n <> tupled (as [])

instance Fn t => Fn (RExpr a -> t) where
  fn' n as a = fn' n (as . (coerce a :))


lit :: Double -> RExpr a
lit = RExpr . pretty
