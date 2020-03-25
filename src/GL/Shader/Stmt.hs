{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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

import           Control.Monad.Trans.Cont
import           Data.Text.Prettyprint.Doc hiding (dot)
import           GL.Shader (Stage(..))
import           GL.Shader.Expr as Expr
import qualified GL.Uniform as GL
import           Prelude hiding (break)

runStmt :: (a -> Doc ()) -> Stmt k a -> Doc ()
runStmt k = (`runCont` k) . getStmt

renderStmt :: Stmt k () -> Doc ()
renderStmt = runStmt (const mempty)

newtype Stmt (k :: Stage) a = Stmt { getStmt :: Cont (Doc ()) a }
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

emitVertex :: Stmt 'Geometry () -> Stmt 'Geometry ()
emitVertex m = stmt
  $  renderStmt m <> hardline
  <> pretty "EmitVertex();" <> hardline

emitPrimitive :: Stmt 'Geometry () -> Stmt 'Geometry ()
emitPrimitive m = stmt
  $  renderStmt m <> hardline
  <> pretty "EndPrimitive();" <> hardline

discard :: Stmt 'Fragment ()
discard = stmt $ pretty "discard" <> pretty ';' <> hardline

renderTypeOf :: forall a expr . GL.Uniform a => expr a -> Doc ()
renderTypeOf _ = pretty (GL.glslType @a)


stmt :: Doc () -> Stmt k ()
stmt d = Stmt . cont $ \ k -> d <> k ()
