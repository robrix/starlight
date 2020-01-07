{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
module GL.Shader.Decl
( Decl
, Primitive.Type(..)
, primitiveIn
, primitiveOut
) where

import           Data.Text.Prettyprint.Doc hiding (dot)
import qualified GL.Primitive as Primitive
import           GL.Shader (Type(..))
import           GL.Shader.Stmt

type Decl = Stmt


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
