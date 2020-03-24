{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module GL.Shader.Stmt
( -- * Statements
  Stmt
  -- * Variables
, let'
, GL.Shader.Stmt.var
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

import           Control.Monad (ap, liftM)
import           Data.Text.Prettyprint.Doc hiding (dot)
import           GL.Shader (Stage(..))
import           GL.Shader.Expr as Expr
import qualified GL.Uniform as GL
import           Prelude hiding (break)

runStmt :: (a -> Doc ()) -> Stmt k a -> Doc ()
runStmt k (Stmt run) = run k

renderStmt :: Stmt k () -> Doc ()
renderStmt = runStmt (const mempty)

newtype Stmt (k :: Stage) a = Stmt ((a -> Doc ()) -> Doc ())

instance Functor (Stmt k) where
  fmap = liftM

instance Applicative (Stmt k) where
  pure a = Stmt $ \ k -> k a
  (<*>) = ap

instance Monad (Stmt k) where
  Stmt m >>= f = Stmt $ \ k -> m (runStmt k . f)

let' :: GL.Uniform a => String -> Render k a -> Stmt k (Render k a)
let' n v = Stmt $ \ k
  -> renderTypeOf v <+> pretty n <+> pretty '=' <+> renderExpr v <> pretty ';' <> hardline
  <> k (Render (pretty n))

var :: GL.Uniform a => String -> Render k a -> Stmt k (Ref k a)
var n v = Stmt $ \ k
  -> renderTypeOf v <+> pretty n <+> pretty '=' <+> renderExpr v <> pretty ';' <> hardline
  <> k (Ref n)

iff :: Render k Bool -> Stmt k () -> Stmt k () -> Stmt k ()
iff c t e = Stmt $ \ k
  -> pretty "if" <+> parens (renderExpr c) <+> braces (nest 2 (line <> renderStmt t <> line)) <+> pretty "else" <+> braces (nest 2 (line <> renderStmt e <> line)) <> hardline
  <> k ()

switch :: Render k Int -> [(Maybe Int, Stmt k ())] -> Stmt k ()
switch s cs = Stmt $ \ k
  -> pretty "switch" <+> parens (renderExpr s) <+> braces (nest 2 (line <> vsep (map renderCase cs) <> line)) <> hardline
  <> k ()
  where
  renderCase (i, s) = maybe (pretty "default:" <> hardline) (\ i -> pretty "case" <+> pretty i <> pretty ':') i  <> hardline <> renderStmt s

break :: Stmt k ()
break = Stmt $ \ k
  -> pretty "break" <+> pretty ';' <> hardline
  <> k ()

while :: Render k Bool -> Stmt k () -> Stmt k ()
while c t = Stmt $ \ k
  -> pretty "while" <+> parens (renderExpr c) <+> braces (nest 2 (line <> renderStmt t <> line)) <> hardline
  <> k ()

(.=) :: Ref k a -> Render k a -> Stmt k ()
r .= v = Stmt $ \ k
  -> renderRef r <+> pretty '=' <+> renderExpr v <> pretty ';' <> hardline
  <> k ()

(+=) :: Ref k a -> Render k a -> Stmt k ()
r += v = Stmt $ \ k
  -> renderRef r <+> pretty "+=" <+> renderExpr v <> pretty ';' <> hardline
  <> k ()

(*=) :: Ref k a -> Render k a -> Stmt k ()
r *= v = Stmt $ \ k
  -> renderRef r <+> pretty "*=" <+> renderExpr v <> pretty ';' <> hardline
  <> k ()

(*!=) :: Ref k (v a) -> Render k (v (v a)) -> Stmt k ()
r *!= v = Stmt $ \ k
  -> renderRef r <+> pretty "*=" <+> renderExpr v <> pretty ';' <> hardline
  <> k ()

infixr 4 .=, +=, *=, *!=

emitVertex :: Stmt 'Geometry () -> Stmt 'Geometry ()
emitVertex m = Stmt $ \ k
  -> renderStmt m <> hardline
  <> pretty "EmitVertex();" <> hardline
  <> k ()

emitPrimitive :: Stmt 'Geometry () -> Stmt 'Geometry ()
emitPrimitive m = Stmt $ \ k
  -> renderStmt m <> hardline
  <> pretty "EndPrimitive();" <> hardline
  <> k ()

discard :: Stmt 'Fragment ()
discard = Stmt $ \ k
  -> pretty "discard" <> pretty ';' <> hardline
  <> k ()

renderTypeOf :: forall a expr . GL.Uniform a => expr a -> Doc ()
renderTypeOf _ = pretty (GL.glslType @a)
