{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}
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
, Decl
, main
, primitiveIn
, primitiveOut
  -- * Stmts
, Stmt
  -- ** Variables
, let'
, var
  -- ** Control flow
, iff
, switch
, break
, while
  -- ** Assignment
, (.=)
, (+=)
, (*=)
, (*!=)
  -- ** Geometry shaders
, emitVertex
, emitPrimitive
  -- ** Fragment shaders
, discard
  -- * References (mutable variables)
, Ref(..)
, gl_Position
, gl_PointSize
, gl_InstanceID
, gl_Positions
, gl_FragCoord
, gl_FrontFacing
, gl_PointCoord
, (^^.)

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
  Shader :: Vars u => ((forall k . u (Expr k)) -> Stage Decl i o) -> Shader u i o

program :: Vars u => ((forall k . u (Expr k)) -> Stage Decl i o) -> Shader u i o
program = Shader


data Stage d i o where
  Id :: Stage d i i
  (:>>>) :: Stage d i x -> Stage d x o -> Stage d i o
  V :: (Vars i, Vars o) => (i (Expr 'Shader.Vertex)   -> o (Ref 'Shader.Vertex)   -> d 'Shader.Vertex   ()) -> Stage d i o
  G :: (Vars i, Vars o) => (i (Expr 'Shader.Geometry :.: []) -> o (Ref 'Shader.Geometry) -> d 'Shader.Geometry ()) -> Stage d i o
  F :: (Vars i, Vars o) => (i (Expr 'Shader.Fragment) -> o (Ref 'Shader.Fragment) -> d 'Shader.Fragment ()) -> Stage d i o

vertex   :: (Vars i, Vars o) => (i (Expr 'Shader.Vertex)   -> o (Ref 'Shader.Vertex)   -> d 'Shader.Vertex   ()) -> Stage d i o
vertex = V

geometry :: (Vars i, Vars o) => (i (Expr 'Shader.Geometry :.: []) -> o (Ref 'Shader.Geometry) -> d 'Shader.Geometry ()) -> Stage d i o
geometry = G

fragment :: (Vars i, Vars o) => (i (Expr 'Shader.Fragment) -> o (Ref 'Shader.Fragment) -> d 'Shader.Fragment ()) -> Stage d i o
fragment = F

instance Cat.Category (Stage d) where
  id = Id
  (.) = flip (:>>>)


data None (v :: Type -> Type) = None
  deriving (Generic)

instance Vars None

shaderSources :: Shader u i o -> [(Shader.Stage, String)]
shaderSources (Shader f) = fmap (renderString . layoutPretty defaultLayoutOptions) <$> stageSources u' (f u) where
  u = makeVars (Expr . pretty . name)
  u' = foldVars (getK . value) (makeVars (pvar "uniform" . name) `like` u)

stageSources :: Doc () -> Stage Decl i o -> [(Shader.Stage, Doc ())]
stageSources u = \case
  Id  -> []
  V s -> [renderStage Shader.Vertex   id s]
  G s -> [renderStage Shader.Geometry (coerce :: forall x . Expr 'Shader.Geometry x -> (Expr 'Shader.Geometry :.: []) x) s]
  F s -> [renderStage Shader.Fragment id s]
  l :>>> r -> stageSources u l <> stageSources u r
  where
  renderStage :: (Vars i, Vars o) => Shader.Stage -> (forall a . Expr k a -> f a) -> (i f -> o (Ref k) -> Decl k ()) -> (Shader.Stage, Doc ())
  renderStage t g f = (,) t
    $  pretty "#version 410" <> hardline
    <> u
    <> case t of
      Shader.Geometry -> foldVars (getK . value) (makeVars (pvar "in" . (<> "[]") . name) `like` i)
      _               -> foldVars (getK . value) (makeVars (pvar "in"      . name) `like` i)
    <> foldVars (getK . value) (makeVars (pvar "out"     . name) `like` o)
    <> renderDecl (f i o) where
    i = makeVars (g . Expr . pretty . name)
    o = makeVars (Ref . name)

like :: t (K a) -> t b -> t (K a)
like = const

pvar :: GL.Uniform a => String -> String -> K (Doc ()) a
pvar qual n = fix $ \ c -> K $ pretty qual <+> renderTypeOf c <+> pretty n <> pretty ';' <> hardline


newtype Frag v = Frag { fragColour :: v (Colour Float) }
  deriving (Generic)

instance Vars Frag


-- Decls

runDecl :: (a -> Doc ()) -> Decl k a -> Doc ()
runDecl k = (`runCont` k) . getDecl

newtype Decl (k :: Shader.Stage) a = Decl { getDecl :: Cont (Doc ()) a }
  deriving (Applicative, Functor, Monad)

raw :: Doc () -> Decl k ()
raw d = Decl . cont $ \ k -> d <> k ()

main :: Stmt k () -> Decl k ()
main body = raw (pretty "void" <+> pretty "main" <> parens mempty <+> braces (nest 2 (line <> renderStmt body <> line)))


primitiveIn :: P.Type -> Decl 'Shader.Geometry ()
primitiveIn ty = raw doc where
  doc = pretty "layout" <+> parens (render ty) <+> pretty "in;" <> hardline
  render = \case
    P.Points        -> pretty "points"
    P.Lines         -> pretty "lines"
    P.LineStrip     -> pretty "lines"
    P.LineLoop      -> pretty "lines"
    P.TriangleStrip -> pretty "triangles"
    P.Triangles     -> pretty "triangles"

primitiveOut :: P.Type -> Int -> Decl 'Shader.Geometry ()
primitiveOut ty mx = raw doc where
  doc = pretty "layout" <+> parens (render ty <> comma <+> pretty "max_vertices" <+> equals <+> pretty mx) <+> pretty "out;" <> hardline
  render = \case
    P.Points        -> pretty "points"
    P.Lines         -> pretty "line_strip"
    P.LineStrip     -> pretty "line_strip"
    P.LineLoop      -> pretty "line_strip"
    P.TriangleStrip -> pretty "triangle_strip"
    P.Triangles     -> pretty "triangle_strip"


renderDecl :: Decl k a -> Doc ()
renderDecl = runDecl (const mempty)


-- Stmts

runStmt :: (a -> Doc ()) -> Stmt k a -> Doc ()
runStmt k = (`runCont` k) . getStmt

renderStmt :: Stmt k () -> Doc ()
renderStmt = runStmt (const mempty)

newtype Stmt (k :: Shader.Stage) a = Stmt { getStmt :: Cont (Doc ()) a }
  deriving (Applicative, Functor, Monad)

let' :: GL.Uniform a => String -> Expr k a -> Stmt k (Expr k a)
let' n v = Stmt . cont $ \ k
  -> renderTypeOf v <+> pretty n <+> pretty '=' <+> renderExpr v <> pretty ';' <> hardline
  <> k (Expr (pretty n))

var :: GL.Uniform a => String -> Expr k a -> Stmt k (Ref k a)
var n v = Stmt . cont $ \ k
  -> renderTypeOf v <+> pretty n <+> pretty '=' <+> renderExpr v <> pretty ';' <> hardline
  <> k (Ref n)

iff :: Expr k Bool -> Stmt k () -> Stmt k () -> Stmt k ()
iff c t e = stmt $ pretty "if" <+> parens (renderExpr c) <+> braces (nest 2 (line <> renderStmt t <> line)) <+> pretty "else" <+> braces (nest 2 (line <> renderStmt e <> line)) <> hardline

switch :: Expr k Int -> [(Maybe Int, Stmt k ())] -> Stmt k ()
switch s cs = stmt $ pretty "switch" <+> parens (renderExpr s) <+> braces (nest 2 (line <> vsep (map renderCase cs) <> line)) <> hardline
  where
  renderCase (i, s) = maybe (pretty "default:" <> hardline) (\ i -> pretty "case" <+> pretty i <> pretty ':') i  <> hardline <> renderStmt s

break :: Stmt k ()
break = stmt $ pretty "break" <+> pretty ';' <> hardline

while :: Expr k Bool -> Stmt k () -> Stmt k ()
while c t = stmt $ pretty "while" <+> parens (renderExpr c) <+> braces (nest 2 (line <> renderStmt t <> line)) <> hardline

(.=) :: Ref k a -> Expr k a -> Stmt k ()
r .= v = stmt $ renderRef r <+> pretty '=' <+> renderExpr v <> pretty ';' <> hardline

(+=) :: Ref k a -> Expr k a -> Stmt k ()
r += v = stmt $ renderRef r <+> pretty "+=" <+> renderExpr v <> pretty ';' <> hardline

(*=) :: Ref k a -> Expr k a -> Stmt k ()
r *= v = stmt $ renderRef r <+> pretty "*=" <+> renderExpr v <> pretty ';' <> hardline

(*!=) :: Ref k (v a) -> Expr k (v (v a)) -> Stmt k ()
r *!= v = stmt $ renderRef r <+> pretty "*=" <+> renderExpr v <> pretty ';' <> hardline

infixr 4 .=, +=, *=, *!=

emitVertex :: Stmt 'Shader.Geometry () -> Stmt 'Shader.Geometry ()
emitVertex m = stmt
  $  renderStmt m <> hardline
  <> pretty "EmitVertex();" <> hardline

emitPrimitive :: Stmt 'Shader.Geometry () -> Stmt 'Shader.Geometry ()
emitPrimitive m = stmt
  $  renderStmt m <> hardline
  <> pretty "EndPrimitive();" <> hardline

discard :: Stmt 'Shader.Fragment ()
discard = stmt $ pretty "discard" <> pretty ';' <> hardline

renderTypeOf :: forall a expr . GL.Uniform a => expr a -> Doc ()
renderTypeOf _ = pretty (GL.glslType @a)


stmt :: Doc () -> Stmt k ()
stmt d = Stmt . cont $ \ k -> d <> k ()


-- Exprs

data Ref (k :: Shader.Stage) t
  = Ref String
  | forall s . Ref k s :^^. Prj s t

gl_Position :: Ref k (V4 Float)
gl_Position = Ref "gl_Position"

gl_PointSize :: Ref 'Shader.Vertex Float
gl_PointSize = Ref "gl_PointSize"

gl_InstanceID :: Expr 'Shader.Vertex Int
gl_InstanceID = Expr $ pretty "gl_InstanceID"


gl_Positions :: Expr 'Shader.Geometry [V4 Float]
gl_Positions = Expr $ pretty "gl_Position"


gl_FragCoord :: Expr 'Shader.Fragment (V2 Float)
gl_FragCoord = Expr $ pretty "gl_FragCoord"

gl_FrontFacing :: Expr 'Shader.Fragment Bool
gl_FrontFacing = Expr $ pretty "gl_FrontFacing"

gl_PointCoord :: Expr 'Shader.Fragment (V2 Float)
gl_PointCoord = Expr $ pretty "gl_PointCoord"


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


newtype Expr (k :: Shader.Stage) a = Expr { renderExpr :: Doc () }

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
