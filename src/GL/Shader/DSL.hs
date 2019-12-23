{-# LANGUAGE AllowAmbiguousTypes, DataKinds, FlexibleInstances, FunctionalDependencies, GADTs, KindSignatures, LambdaCase, ScopedTypeVariables, TypeApplications, TypeOperators, UndecidableInstances #-}
module GL.Shader.DSL
( Shader
, Stmt
, Expr
, Ref
, Prj
, main
, let'
, vec2
, vec3
, vec4
, len
, coerce
, gl_Position
, gl_PointSize
, gl_PointCoord
, discard
, iff
, gt
, (.=)
, (^.)
, _x
, _y
, _z
, _w
, _xy
, _xyz
, _a
, (^*)
, (!*)
, renderShader
, renderStmt
, renderExpr
, GLSLType(..)
, Mk(..)
, (:::)(..)
, Type(..)
) where

import Control.Monad ((<=<), ap, liftM)
import qualified Data.Coerce as C
import Data.DSL
import Data.Proxy
import Data.Text.Prettyprint.Doc
import GHC.TypeLits
import GL.Shader (Type(..))
import Linear.Matrix (M33)
import Linear.V2 (V2(..))
import Linear.V3 (V3(..))
import Linear.V4 (V4(..))
import UI.Colour (Colour)
import Unit.Angle

data Shader (k :: Type) (u :: Context) (i :: Context) (o :: Context) where
  Uniform :: (KnownSymbol n, GLSLType t) => Shader k u i o -> Shader k (n '::: t ': u) i o
  Input   :: (KnownSymbol n, GLSLType t) => Shader k u i o -> Shader k u (n '::: t ': i) o
  Output  :: (KnownSymbol n, GLSLType t) => Shader k u i o -> Shader k u i (n '::: t ': o)
  Main    :: Stmt k () -> Shader k u i o


data Stmt (k :: Type) a where
  Pure :: a -> Stmt k a
  Let :: GLSLType b => String -> Expr k b -> (Expr k b -> Stmt k a) -> Stmt k a
  Discard :: Stmt 'Fragment a -> Stmt 'Fragment a
  If :: Expr k Bool -> Stmt k () -> Stmt k () -> Stmt k a -> Stmt k a
  (:.=) :: Ref k b -> Expr k b -> Stmt k a -> Stmt k a
  Stmt :: Pretty b => b -> (b -> Stmt k a) -> Stmt k a

infixr 4 :.=

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
  (:.=) r v k >>= f = (r :.= v) (k >>= f)
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

  Gt :: Expr k a -> Expr k a -> Expr k Bool

  Vec2 :: Expr k Float -> Expr k Float -> Expr k (V2 Float)
  Vec3 :: Expr k (V2 Float) -> Expr k Float -> Expr k (V3 Float)
  Vec4 :: Expr k (V3 Float) -> Expr k Float -> Expr k (V4 Float)
  Len :: Expr k (v Float) -> Expr k Float

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
  fromInteger = Lit . fromInteger

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


main :: Stmt k () -> Shader k u i o
main = Main


let' :: GLSLType a => String -> Expr k a -> Stmt k (Expr k a)
let' n v = Let n v pure


vec2 :: Expr k Float -> Expr k Float -> Expr k (V2 Float)
vec2 = Vec2

vec3 :: Expr k (V2 Float) -> Expr k Float -> Expr k (V3 Float)
vec3 = Vec3

vec4 :: Expr k (V3 Float) -> Expr k Float -> Expr k (V4 Float)
vec4 = Vec4

len :: Expr k (v Float) -> Expr k Float
len = Len


coerce :: C.Coercible a b => Expr k a -> Expr k b
coerce = Coerce


gl_Position :: Ref 'Vertex (V4 Float)
gl_Position = Ref "gl_Position"

gl_PointSize :: Ref 'Vertex Float
gl_PointSize = Ref "gl_PointSize"


gl_PointCoord :: Expr 'Fragment (V2 Float)
gl_PointCoord = Var "gl_PointCoord"

discard :: Stmt 'Fragment ()
discard = Discard (pure ())


iff :: Expr k Bool -> Stmt k () -> Stmt k () -> Stmt k ()
iff c t e = If c t e (pure ())

gt :: Expr k Float -> Expr k Float -> Expr k Bool
gt = Gt

infix 4 `gt`


(.=) :: Ref k a -> Expr k a -> Stmt k ()
r .= v = (r :.= v) (pure ())

infixr 4 .=

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

_xyz :: Prj (v a) (V3 a)
_xyz = Prj "xyz"

_a :: Prj (v a) a
_a = Prj "a"


(^*) :: Expr k (v a) -> Expr k a -> Expr k (v a)
(^*) = (:^*)

infixl 7 ^*


(!*) :: Expr k (M33 Float) -> Expr k (V3 Float) -> Expr k (V3 Float)
(!*) = (:!*)

infixl 7 !*


renderShader :: Shader k u i o -> Doc ()
renderShader s = pretty "#version 410" <> hardline <> go s where
  go :: Shader k u i o -> Doc ()
  go = \case
    s@(Uniform k)
      -> pretty "uniform" <+> renderTypeOf (typeOf (uniformsOf s)) <+> pretty (symbolVal (nameOf (uniformsOf s))) <> pretty ';' <> hardline
      <> go k
    s@(Input k)
      -> pretty "in" <+> renderTypeOf (typeOf (inputsOf s)) <+> pretty (symbolVal (nameOf (inputsOf s))) <> pretty ';' <> hardline
      <> go k
    s@(Output k)
      -> pretty "out" <+> renderTypeOf (typeOf (outputsOf s)) <+> pretty (symbolVal (nameOf (outputsOf s))) <> pretty ';' <> hardline
      <> go k
    Main s -> pretty "void" <+> pretty "main" <> parens mempty <+> braces (nest 2 (line <> renderStmt s <> line))
  uniformsOf :: Shader k u i o -> Proxy u
  uniformsOf _ = Proxy
  inputsOf :: Shader k u i o -> Proxy i
  inputsOf _ = Proxy
  outputsOf :: Shader k u i o -> Proxy o
  outputsOf _ = Proxy
  typeOf :: Proxy ((n '::: t ': us) :: Context) -> Proxy t
  typeOf _ = Proxy
  nameOf :: Proxy ((n '::: t ': us) :: Context) -> Proxy n
  nameOf _ = Proxy

renderStmt :: Pretty a => Stmt k a -> Doc ()
renderStmt = \case
  Pure a -> pretty a
  Let n v k
    -> renderTypeOf v <+> pretty n <+> pretty '=' <+> renderExpr v <> pretty ';' <> hardline
    <> renderStmt (k (Var n))
  Discard k
    -> pretty "discard" <> pretty ';' <> hardline
    <> renderStmt k
  If c t e k
    -> pretty "if" <+> parens (renderExpr c) <+> braces (nest 2 (line <> renderStmt t <> line)) <+> pretty "else" <+> braces (nest 2 (line <> renderStmt e <> line)) <> hardline
    <> renderStmt k
  (:.=) (Ref r) v k
    -> pretty r <+> pretty '=' <+> renderExpr v <> pretty ';' <> hardline
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
  Gt a b -> renderExpr a <+> pretty '>' <+> renderExpr b
  Vec2 a b -> fn "vec2" [renderExpr a, renderExpr b]
  Vec3 a b -> fn "vec3" [renderExpr a, renderExpr b]
  Vec4 a b -> fn "vec4" [renderExpr a, renderExpr b]
  Len a -> fn "length" [renderExpr a]
  Coerce a -> renderExpr a
  where
  fn n as = pretty n <> tupled as

class GLSLType a where
  renderTypeOf :: expr a -> Doc ()

instance GLSLType (Radians Float) where
  renderTypeOf _ = pretty "float"

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


_radarVertex
  :: Shader
    'Vertex
    '[ "matrix" '::: M33 Float
     , "angle"  '::: Radians Float
     , "sweep"  '::: Radians Float
     ]
    '[ "n" '::: Float ]
    '[]
_radarVertex = mk $ \ matrix angle sweep n -> do
  angle <- let' "angle" (coerce angle + n * coerce sweep)
  pos <- let' "pos" (vec2 (cos angle) (sin angle) ^* 150)
  gl_Position .= vec4 (vec3 ((matrix !* vec3 pos 1) ^. _xy) 0) 1

_pointsVertex
  :: Shader
    'Vertex
    '[ "matrix" '::: M33 Float
     , "pointSize" '::: Float
     ]
    '[ "pos" '::: V2 Float ]
    '[]
_pointsVertex = mk $ \ matrix pointSize pos -> do
  gl_Position .= vec4 (vec3 ((matrix !* vec3 pos 1) ^. _xy) 0) 1
  gl_PointSize .= pointSize

_pointsFragment
  :: Shader
    'Fragment
    '[ "colour"     '::: Colour Float ]
    '[]
    '[ "fragColour" '::: Colour Float ]
_pointsFragment = mk $ \ colour fragColour -> do
  p <- let' "p" (gl_PointCoord - vec2 0.5 0.5)
  iff (len p `gt` 1)
    discard
    (do
      mag <- let' "mag" (len p * 2)
      fragColour .= vec4 (colour ^. _xyz) (1 - mag * mag * mag / 2))

_shipVertex
  :: Shader
    'Vertex
    '[ "matrix" '::: M33 Float ]
    '[ "position2" '::: V2 Float ]
    '[]
_shipVertex = mk $ \ matrix pos ->
  gl_Position .= vec4 (matrix !* vec3 pos 1) 1

_shipFragment
  :: Shader
    'Fragment
    '[ "colour"     '::: Colour Float ]
    '[]
    '[ "fragColour" '::: Colour Float ]
_shipFragment = mk $ \ colour fragColour ->
  fragColour .= colour


class Mk k u i o a | k u i o -> a where
  mk :: a -> Shader k u i o

instance Mk k '[] '[] '[] (Stmt k ()) where
  mk = Main

instance {-# OVERLAPPABLE #-} (KnownSymbol n, GLSLType t, Mk k '[] '[] os a) => Mk k '[] '[] (n '::: t ': os) (Ref k t -> a) where
  mk f = Output (mk (f (Ref (symbolVal (Proxy @n)))))

instance {-# OVERLAPPABLE #-} (KnownSymbol n, GLSLType t, Mk k '[] is os a) => Mk k '[] (n '::: t ': is) os (Expr k t -> a) where
  mk f = Input (mk (f (Var (symbolVal (Proxy @n)))))

instance {-# OVERLAPPABLE #-} (KnownSymbol n, GLSLType t, Mk k us is os a) => Mk k (n '::: t ': us) is os (Expr k t -> a) where
  mk f = Uniform (mk (f (Var (symbolVal (Proxy @n)))))
