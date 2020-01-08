{-# LANGUAGE DataKinds #-}
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

import           Control.Monad (ap, liftM)
import           Data.Text.Prettyprint.Doc hiding (dot)
import qualified GL.Primitive as P
import           GL.Shader (Type(..))
import           GL.Shader.Stmt

runDecl :: (a -> Doc ()) -> Decl k a -> Doc ()
runDecl k (Decl run) = run k

newtype Decl (k :: Type) a = Decl ((a -> Doc ()) -> Doc ())

instance Functor (Decl k) where
  fmap = liftM

instance Applicative (Decl k) where
  pure a = Decl (\ k -> k a)
  (<*>) = ap

instance Monad (Decl k) where
  Decl m >>= f = Decl (\ k -> m (runDecl k . f))

raw :: Doc () -> Decl k ()
raw d = Decl (\ k -> d <> k ())

main :: Stmt k () -> Decl k ()
main body = raw (pretty "void" <+> pretty "main" <> parens mempty <+> braces (nest 2 (line <> renderStmt body <> line)))


primitiveIn :: P.Type -> Decl 'Geometry ()
primitiveIn ty = raw doc where
  doc = pretty "layout" <+> parens (render ty) <+> pretty "in;" <> hardline
  render = \case
    P.Points -> pretty "points"
    P.Lines -> pretty "lines"
    P.LineStrip -> pretty "lines"
    P.LineLoop -> pretty "lines"
    P.TriangleStrip -> pretty "triangles"
    P.Triangles -> pretty "triangles"

primitiveOut :: P.Type -> Int -> Decl 'Geometry ()
primitiveOut ty mx = raw doc where
  doc = pretty "layout" <+> parens (render ty <> comma <+> pretty "max_vertices" <+> equals <+> pretty mx) <+> pretty "out;" <> hardline
  render = \case
    P.Points -> pretty "points"
    P.Lines -> pretty "line_strip"
    P.LineStrip -> pretty "line_strip"
    P.LineLoop -> pretty "line_strip"
    P.TriangleStrip -> pretty "triangle_strip"
    P.Triangles -> pretty "triangle_strip"


renderDecl :: Decl k a -> Doc ()
renderDecl = runDecl (const mempty)
