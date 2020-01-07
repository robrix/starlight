{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
module GL.Shader.Decl
( Decl
, main
, Primitive.Type(..)
, primitiveIn
, primitiveOut
  -- * Pretty-printing
, renderDecl
) where

import           Control.Monad (ap, liftM, (<=<))
import           Data.Text.Prettyprint.Doc hiding (dot)
import qualified GL.Primitive as Primitive
import           GL.Shader (Type(..))
import           GL.Shader.Stmt

data Decl (k :: Type) a where
  Pure :: a -> Decl k a
  Raw :: Doc () -> Decl k a -> Decl k a
  Decl :: Pretty b => b -> (b -> Decl k a) -> Decl k a

instance Functor (Decl k) where
  fmap = liftM

instance Applicative (Decl k) where
  pure = Pure
  (<*>) = ap

instance Monad (Decl k) where
  Pure a            >>= f = f a
  Raw d           k >>= f = Raw d (k >>= f)
  Decl a          k >>= f = Decl a (f <=< k)


main :: Stmt k () -> Decl k ()
main body = Raw (pretty "void" <+> pretty "main" <> parens mempty <+> braces (nest 2 (line <> renderStmt body <> line))) (pure ())


primitiveIn :: Primitive.Type -> Decl 'Geometry ()
primitiveIn ty = Raw (render ty) (pure ()) where
  render = (pretty "layout" <+>) . (<+> pretty "in;" <> hardline) . parens . \case
    Primitive.Points -> pretty "points"
    Primitive.Lines -> pretty "lines"
    Primitive.LineStrip -> pretty "lines"
    Primitive.LineLoop -> pretty "lines"
    Primitive.TriangleStrip -> pretty "triangles"
    Primitive.Triangles -> pretty "triangles"

primitiveOut :: Primitive.Type -> Decl 'Geometry ()
primitiveOut ty = Raw (render ty) (pure ()) where
  render = (pretty "layout" <+>) . (<+> pretty "out;" <> hardline) . parens . (<> comma <+> pretty "max_vertices = 256") . \case
    Primitive.Points -> pretty "points"
    Primitive.Lines -> pretty "line_strip"
    Primitive.LineStrip -> pretty "line_strip"
    Primitive.LineLoop -> pretty "line_strip"
    Primitive.TriangleStrip -> pretty "triangle_strip"
    Primitive.Triangles -> pretty "triangle_strip"


renderDecl :: Decl k a -> Doc ()
renderDecl = \case
  Pure _ -> mempty
  Raw d k
    -> d
    <> renderDecl k
  Decl b k
    -> pretty b <> pretty ';' <> hardline
    <> renderDecl (k b)
