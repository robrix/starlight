{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
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
  -- * Exprs
, module GL.Shader.Expr
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
import           GL.Shader.Expr
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
