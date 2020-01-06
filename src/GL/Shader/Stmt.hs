{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module GL.Shader.Stmt
( -- * Statements
  Stmt
  -- * Variables
, let'
, var
  -- * Control flow
, iff
, switch
, break
, while
  -- * Assignment
, (.=)
, (+=)
, (*=)
, (*!=)
  -- * Geometry shaders
, emitVertex
, emitPrimitive
  -- * Fragment shaders
, discard
  -- * Pretty-printing
, renderStmt
, renderTypeOf
) where

import           Control.Monad (ap, liftM, (<=<))
import           Data.Functor.Const
import           Data.Text.Prettyprint.Doc hiding (dot)
import           GL.Shader (Type(..))
import           GL.Shader.Expr
import qualified GL.Uniform as GL
import           Prelude hiding (break)

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
  EmitVertex :: Stmt 'Geometry () -> Stmt 'Geometry a -> Stmt 'Geometry a
  EmitPrimitive :: Stmt 'Geometry () -> Stmt 'Geometry a -> Stmt 'Geometry a
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
  Pure a            >>= f = f a
  Let n v         k >>= f = Let n v (f <=< k)
  Discard         k >>= f = Discard (k >>= f)
  If c t e        k >>= f = If c t e (k >>= f)
  Switch s c      k >>= f = Switch s c (k >>= f)
  Break           k >>= f = Break (k >>= f)
  While c t       k >>= f = While c t (k >>= f)
  (:.=) r v       k >>= f = (r :.= v) (k >>= f)
  (:+=) r v       k >>= f = (r :+= v) (k >>= f)
  (:*=) r v       k >>= f = (r :*= v) (k >>= f)
  (:*!=) r v      k >>= f = (r :*!= v) (k >>= f)
  EmitVertex    m k >>= f = EmitVertex m (k >>= f)
  EmitPrimitive m k >>= f = EmitPrimitive m (k >>= f)
  Stmt a          k >>= f = Stmt a (f <=< k)


let' :: GL.Uniform a => String -> Expr k a -> Stmt k (Expr k a)
let' n v = Let n v (pure . Var . getConst)

var :: GL.Uniform a => String -> Expr k a -> Stmt k (Ref k a)
var n v = Let n v (pure . Ref . getConst)


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


emitVertex :: Stmt 'Geometry () -> Stmt 'Geometry ()
emitVertex m = EmitVertex m (pure ())

emitPrimitive :: Stmt 'Geometry () -> Stmt 'Geometry ()
emitPrimitive m = EmitPrimitive m (pure ())


discard :: Stmt 'Fragment ()
discard = Discard (pure ())


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
  EmitVertex m k
    -> renderStmt m <> hardline
    <> pretty "EmitVertex();" <> hardline
    <> renderStmt k
  EmitPrimitive m k
    -> renderStmt m <> hardline
    <> pretty "EndPrimitive();" <> hardline
    <> renderStmt k
  Stmt b k
    -> pretty b <> pretty ';' <> hardline
    <> renderStmt (k b)
  where
  renderCase (i, s) = maybe (pretty "default:" <> hardline) (\ i -> pretty "case" <+> pretty i <> pretty ':') i  <> hardline <> renderStmt s

renderTypeOf :: forall a expr . GL.Uniform a => expr a -> Doc ()
renderTypeOf _ = pretty (GL.glslType @a)
