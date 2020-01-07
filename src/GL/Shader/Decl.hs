{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
module GL.Shader.Decl
( Decl
, main
, P.Type(..)
, primitiveIn
, primitiveOut
  -- * Pretty-printing
, renderDecl
) where

import           Control.Monad (ap, liftM, (<=<))
import           Data.Text.Prettyprint.Doc hiding (dot)
import qualified GL.Primitive as P
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
  Pure a   >>= f = f a
  Raw  d k >>= f = Raw  d (f =<< k)
  Decl a k >>= f = Decl a (f <=< k)


main :: Stmt k () -> Decl k ()
main body = Raw (pretty "void" <+> pretty "main" <> parens mempty <+> braces (nest 2 (line <> renderStmt body <> line))) (pure ())


primitiveIn :: P.Type -> Decl 'Geometry ()
primitiveIn ty = Raw doc (pure ()) where
  doc = pretty "layout" <+> parens (render ty) <+> pretty "in;" <> hardline
  render = \case
    P.Points -> pretty "points"
    P.Lines -> pretty "lines"
    P.LineStrip -> pretty "lines"
    P.LineLoop -> pretty "lines"
    P.TriangleStrip -> pretty "triangles"
    P.Triangles -> pretty "triangles"

primitiveOut :: P.Type -> Int -> Decl 'Geometry ()
primitiveOut ty mx = Raw doc (pure ()) where
  doc = pretty "layout" <+> parens (render ty <> comma <+> pretty "max_vertices" <+> equals <+> pretty mx) <+> pretty "out;" <> hardline
  render = \case
    P.Points -> pretty "points"
    P.Lines -> pretty "line_strip"
    P.LineStrip -> pretty "line_strip"
    P.LineLoop -> pretty "line_strip"
    P.TriangleStrip -> pretty "triangle_strip"
    P.Triangles -> pretty "triangle_strip"


renderDecl :: Decl k a -> Doc ()
renderDecl = \case
  Pure _ -> mempty
  Raw d k
    -> d
    <> renderDecl k
  Decl b k
    -> pretty b <> pretty ';' <> hardline
    <> renderDecl (k b)
