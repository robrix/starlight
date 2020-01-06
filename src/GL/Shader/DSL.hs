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
, geometry
, fragment
, (Cat.>>>)
, None(..)
, shaderSources
, Stmt
, let'
, var
, discard
, iff
, switch
, break
, while
, (.=)
, (+=)
, (*=)
, (*!=)
, module GL.Shader.Expr
, renderStmt
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
import           Data.Function (fix)
import           Data.Functor.Const
import           Data.Text.Prettyprint.Doc hiding (dot)
import           Data.Text.Prettyprint.Doc.Render.String
import           GHC.Generics
import           GL.Shader (Type(..))
import           GL.Shader.Expr
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
  G :: (Vars i, Vars o) => (i (Expr 'Geometry) -> o (Ref 'Geometry) -> Stmt 'Geometry ()) -> Stage i o
  F :: (Vars i, Vars o) => (i (Expr 'Fragment) -> o (Ref 'Fragment) -> Stmt 'Fragment ()) -> Stage i o

vertex   :: (Vars i, Vars o) => (i (Expr 'Vertex)   -> o (Ref 'Vertex)   -> Stmt 'Vertex   ()) -> Stage i o
vertex = V

geometry :: (Vars i, Vars o) => (i (Expr 'Geometry) -> o (Ref 'Geometry) -> Stmt 'Geometry ()) -> Stage i o
geometry = G

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
  G s -> [(Geometry, renderStage s)]
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


let' :: GL.Uniform a => String -> Expr k a -> Stmt k (Expr k a)
let' n v = Let n v (pure . Var . getConst)

var :: GL.Uniform a => String -> Expr k a -> Stmt k (Ref k a)
var n v = Let n v (pure . Ref . getConst)



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
