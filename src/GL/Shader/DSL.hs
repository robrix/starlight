{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# HLINT ignore "Use camelCase" #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
module GL.Shader.DSL
( Shader
, program
, Stage
, vertex
, geometry
, fragment
, (Cat.>>>)
, None(..)
, shaderSources
, Frag(..)
  -- * Decls
, RDecl
, Decl(..)
  -- * Stmts
, RStmt
, Stmt(..)
, FragmentStmt(..)
  -- * References (mutable variables)
, RRef(..)
, Ref(..)
, VertexRef(..)

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
, RExpr(..)
, Expr(..)
, VertexExpr(..)
, FragmentExpr(..)
  -- * Re-exports
, Fields(..)
, Vars
, Colour
, M22
, M33
, M44
, TextureUnit
, V2
, V3
, V4
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

data Shader (u :: (Type -> Type) -> Type) (i :: (Type -> Type) -> Type) (o :: (Type -> Type) -> Type) where
  Shader :: Vars u => ((forall k . u (RExpr k)) -> Stage RDecl i o) -> Shader u i o

program :: Vars u => ((forall k . u (RExpr k)) -> Stage RDecl i o) -> Shader u i o
program = Shader


data Stage d i o where
  Id :: Stage d i i
  (:>>>) :: Stage d i x -> Stage d x o -> Stage d i o
  V :: (Vars i, Vars o) => (i (RExpr 'Shader.Vertex)   -> o (RRef 'Shader.Vertex)   -> d 'Shader.Vertex   ()) -> Stage d i o
  G :: (Vars i, Vars o) => (i (RExpr 'Shader.Geometry :.: []) -> o (RRef 'Shader.Geometry) -> d 'Shader.Geometry ()) -> Stage d i o
  F :: (Vars i, Vars o) => (i (RExpr 'Shader.Fragment) -> o (RRef 'Shader.Fragment) -> d 'Shader.Fragment ()) -> Stage d i o

vertex   :: (Vars i, Vars o) => (i (RExpr 'Shader.Vertex)   -> o (RRef 'Shader.Vertex)   -> d 'Shader.Vertex   ()) -> Stage d i o
vertex = V

geometry :: (Vars i, Vars o) => (i (RExpr 'Shader.Geometry :.: []) -> o (RRef 'Shader.Geometry) -> d 'Shader.Geometry ()) -> Stage d i o
geometry = G

fragment :: (Vars i, Vars o) => (i (RExpr 'Shader.Fragment) -> o (RRef 'Shader.Fragment) -> d 'Shader.Fragment ()) -> Stage d i o
fragment = F

instance Cat.Category (Stage d) where
  id = Id
  (.) = flip (:>>>)


data None (v :: Type -> Type) = None
  deriving (Generic)

instance Vars None

shaderSources :: Shader u i o -> [(Shader.Stage, String)]
shaderSources (Shader f) = fmap (renderString . layoutPretty defaultLayoutOptions) <$> stageSources u' (f u) where
  u = makeVars (RExpr . pretty . name)
  u' = foldVars (getK . value) (makeVars (pvar "uniform" . name) `like` u)

stageSources :: Doc () -> Stage RDecl i o -> [(Shader.Stage, Doc ())]
stageSources u = \case
  Id  -> []
  V s -> [renderStage Shader.Vertex   id s]
  G s -> [renderStage Shader.Geometry (coerce :: forall x . RExpr 'Shader.Geometry x -> (RExpr 'Shader.Geometry :.: []) x) s]
  F s -> [renderStage Shader.Fragment id s]
  l :>>> r -> stageSources u l <> stageSources u r
  where
  renderStage :: (Vars i, Vars o) => Shader.Stage -> (forall a . RExpr k a -> f a) -> (i f -> o (RRef k) -> RDecl k ()) -> (Shader.Stage, Doc ())
  renderStage t g f = (,) t
    $  pretty "#version 410" <> hardline
    <> u
    <> case t of
      Shader.Geometry -> foldVars (getK . value) (makeVars (pvar "in" . (<> "[]") . name) `like` i)
      _               -> foldVars (getK . value) (makeVars (pvar "in"      . name) `like` i)
    <> foldVars (getK . value) (makeVars (pvar "out"     . name) `like` o)
    <> renderDecl (f i o) where
    i = makeVars (g . RExpr . pretty . name)
    o = makeVars (Ref . name)

like :: t (K a) -> t b -> t (K a)
like = const

pvar :: GL.Uniform a => String -> String -> K (Doc ()) a
pvar qual n = fix $ \ c -> K $ pretty qual <+> renderTypeOf c <+> pretty n <> pretty ';' <> hardline


newtype Frag v = Frag { fragColour :: v (Colour Float) }
  deriving (Generic)

instance Vars Frag


-- Decls

renderDecl :: RDecl k a -> Doc ()
renderDecl = (`runCont` const mempty) . getDecl

newtype RDecl (k :: Shader.Stage) a = RDecl { getDecl :: Cont (Doc ()) a }
  deriving (Applicative, Functor, Monad)

decl :: Doc () -> RDecl k ()
decl d = RDecl . cont $ \ k -> d <> k ()

class Stmt ref expr stmt => Decl ref expr stmt decl | decl -> stmt where
  main :: stmt k () -> decl k ()
  primitiveIn :: P.Type -> decl 'Shader.Geometry ()
  primitiveOut :: P.Type -> Int -> decl 'Shader.Geometry ()

instance Decl RRef RExpr RStmt RDecl where
  main body = decl (pretty "void" <+> pretty "main" <> parens mempty <+> braces (nest 2 (line <> renderStmt body <> line)))

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


-- Stmts

renderStmt :: RStmt k () -> Doc ()
renderStmt = (`runCont` const mempty) . getStmt

newtype RStmt (k :: Shader.Stage) a = RStmt { getStmt :: Cont (Doc ()) a }
  deriving (Applicative, Functor, Monad)

class Expr ref expr => Stmt ref expr stmt | stmt -> expr ref where
  let' :: GL.Uniform a => String -> expr k a -> stmt k (expr k a)
  var :: GL.Uniform a => String -> expr k a -> stmt k (ref k a)

  iff :: expr k Bool -> stmt k () -> stmt k () -> stmt k ()
  switch :: expr k Int -> [(Maybe Int, stmt k ())] -> stmt k ()
  break :: stmt k ()
  while :: expr k Bool -> stmt k () -> stmt k ()

  (.=) :: ref k a -> expr k a -> stmt k ()
  (+=) :: ref k a -> expr k a -> stmt k ()
  (*=) :: ref k a -> expr k a -> stmt k ()
  (*!=) :: ref k (v a) -> expr k (v (v a)) -> stmt k ()

  infixr 4 .=, +=, *=, *!=

  emitVertex :: stmt 'Shader.Geometry () -> stmt 'Shader.Geometry ()
  emitPrimitive :: stmt 'Shader.Geometry () -> stmt 'Shader.Geometry ()

class (FragmentExpr ref expr, Stmt ref expr stmt) => FragmentStmt ref expr stmt where
  discard :: stmt 'Shader.Fragment ()

instance Stmt RRef RExpr RStmt where
  let' n v = RStmt . cont $ \ k
    -> renderTypeOf v <+> pretty n <+> pretty '=' <+> renderExpr v <> pretty ';' <> hardline
    <> k (RExpr (pretty n))

  var n v = RStmt . cont $ \ k
    -> renderTypeOf v <+> pretty n <+> pretty '=' <+> renderExpr v <> pretty ';' <> hardline
    <> k (Ref n)

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


stmt :: Doc () -> RStmt k ()
stmt d = RStmt . cont $ \ k -> d <> k ()


-- Exprs

data RRef (k :: Shader.Stage) t
  = Ref String
  | forall s . RRef k s :^^. Prj s t

class Ref (ref :: Shader.Stage -> Type -> Type) where
  gl_Position :: ref k (V4 Float)

  (^^.) :: ref k a -> Prj a b -> ref k b
  infixl 8 ^^.

class Ref ref => VertexRef ref where
  gl_PointSize :: ref 'Shader.Vertex Float

instance Ref RRef where
  gl_Position = Ref "gl_Position"

  (^^.) = (:^^.)

instance VertexRef RRef where
  gl_PointSize = Ref "gl_PointSize"

renderRef :: RRef k a -> Doc ()
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


class Ref ref => Expr ref expr | expr -> ref where
  get :: ref k a -> expr k a

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

  (!) :: (expr k :.: []) a -> expr k Int -> expr k a
  infixl 9 !

class (VertexRef ref, Expr ref expr) => VertexExpr ref expr where
  gl_InstanceID :: expr 'Shader.Vertex Int

class Expr ref expr => FragmentExpr ref expr where
  gl_FragCoord :: expr 'Shader.Fragment (V2 Float)
  gl_FrontFacing :: expr 'Shader.Fragment Bool
  gl_PointCoord :: expr 'Shader.Fragment (V2 Float)

newtype RExpr (k :: Shader.Stage) a = RExpr { renderExpr :: Doc () }

instance Num (RExpr k a) where
  a + b = RExpr . parens $ renderExpr a <+> pretty '+' <+> renderExpr b
  a * b = RExpr . parens $ renderExpr a <+> pretty '*' <+> renderExpr b
  a - b = RExpr . parens $ renderExpr a <+> pretty '-' <+> renderExpr b
  signum a = fn "sign" [ renderExpr a ]
  negate a = RExpr . parens $ pretty "-" <> renderExpr a
  abs a = fn "abs" [ renderExpr a ]
  fromInteger i = RExpr $ pretty i

instance Fractional (RExpr k a) where
  a / b = RExpr $ parens $ renderExpr a <+> pretty '/' <+> renderExpr b
  fromRational = lit . fromRational

instance Floating (RExpr k a) where
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

instance Expr RRef RExpr where
  get = RExpr . renderRef

  a ^. Prj s = RExpr $ renderExpr a <> pretty s
  a ^*  b = RExpr . parens $ renderExpr a <+> pretty '*' <+> renderExpr b
  a ^/  b = RExpr . parens $ renderExpr a <+> pretty '/' <+> renderExpr b
  a !*  b = RExpr . parens $ renderExpr a <+> pretty '*' <+> renderExpr b
  a !!*  b = RExpr . parens $ renderExpr a <+> pretty '*' <+> renderExpr b
  a !*! b = RExpr . parens $ renderExpr a <+> pretty '*' <+> renderExpr b
  eq a b = RExpr . parens $ renderExpr a <+> pretty "==" <+> renderExpr b
  lt a b = RExpr . parens $ renderExpr a <+> pretty '<' <+> renderExpr b
  gt a b = RExpr . parens $ renderExpr a <+> pretty '>' <+> renderExpr b

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

  (!) (C v) n = RExpr $ renderExpr v <> brackets (renderExpr n)

instance VertexExpr RRef RExpr where
  gl_InstanceID = RExpr $ pretty "gl_InstanceID"

instance FragmentExpr RRef RExpr where
  gl_FragCoord = RExpr $ pretty "gl_FragCoord"
  gl_FrontFacing = RExpr $ pretty "gl_FrontFacing"
  gl_PointCoord = RExpr $ pretty "gl_PointCoord"

fn :: String -> [Doc ()] -> RExpr k b
fn n as = RExpr $ pretty n <> tupled as

lit :: Double -> RExpr k a
lit = RExpr . pretty
