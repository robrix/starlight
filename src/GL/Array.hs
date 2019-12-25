{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
module GL.Array
( Array(..)
, configureArray
, configureInputs
, Mode(..)
, drawArrays
, loadVertices
, load
) where

import           Control.Algebra
import           Control.Effect.Finally
import           Control.Monad (unless)
import           Control.Monad.IO.Class.Lift
import           Data.Coerce
import           Data.Functor.Identity
import           Data.Functor.Product
import           Data.Interval
import           Data.Monoid (Ap(..))
import           Foreign.Ptr
import qualified Foreign.C.String.Lift as C
import qualified Foreign.Storable as S
import           GHC.Stack
import qualified GL.Buffer as B
import           GL.Enum as GL
import           GL.Error
import           GL.Object
import           GL.Program
import qualified GL.Shader.DSL as DSL
import qualified GL.Type as GL
import           Graphics.GL.Core41
import           Graphics.GL.Types

newtype Array n = Array { unArray :: GLuint }
  deriving (S.Storable)

instance Object (Array n) where
  gen n = runLiftIO . glGenVertexArrays n . coerce
  delete n = runLiftIO . glDeleteVertexArrays n . coerce

instance Bind (Array n) where
  bind = checkingGLError . runLiftIO . glBindVertexArray . maybe 0 unArray


configureArray :: (GL.Type a, Has (Lift IO) sig m) => B.Buffer 'B.Array a -> Array a -> m ()
configureArray _ a = runLiftIO $ do
  glEnableVertexAttribArray 0
  glVertexAttribPointer 0 (GL.glDims a) (GL.glType a) GL_FALSE 0 nullPtr

configureInputs :: (DSL.Vars i, HasProgram u i o m, Has (Lift IO) sig m) => i (Product (B.Buffer 'B.Array) Array) -> m ()
configureInputs v = askProgram >>= \ (Program p) ->
  getAp (DSL.foldVars (\ s (Pair b a) -> Ap . runLiftIO $ do
    loc <- C.withCString s (checkingGLError . glGetAttribLocation p)
    unless (loc < 0) $ do
      bind (Just a)
      bind (Just b)
      checkingGLError $ glEnableVertexAttribArray (fromIntegral loc)
      checkingGLError $ glVertexAttribPointer (fromIntegral loc) (GL.glDims a) (GL.glType a) GL_FALSE 0 nullPtr) v)


data Mode
  = Points
  | Lines
  | LineStrip
  | LineLoop
  | TriangleStrip
  | Triangles
  deriving (Eq, Show)

instance GL.Enum Mode where
  glEnum = \case
    Points        -> GL_POINTS
    Lines         -> GL_LINES
    LineStrip     -> GL_LINE_STRIP
    LineLoop      -> GL_LINE_LOOP
    TriangleStrip -> GL_TRIANGLE_STRIP
    Triangles     -> GL_TRIANGLES


drawArrays
  :: ( Has (Lift IO) sig m
     , HasCallStack
     )
  => Mode
  -> Interval Int
  -> m ()
drawArrays mode i = checkingGLError . runLiftIO $ glDrawArrays (glEnum mode) (fromIntegral (min_ i)) (fromIntegral (size i))


loadVertices :: (GL.Type a, Has Finally sig m, Has (Lift IO) sig m) => [a] -> m (Array a)
loadVertices vertices = do
  buffer <- gen1
  array  <- gen1

  bind (Just buffer)
  B.realloc buffer (length vertices) B.Static B.Draw
  B.copy buffer 0 vertices

  bind (Just array)
  array <$ configureArray buffer array


load :: (DSL.Vars i, Has Finally sig m, Has (Lift IO) sig m) => Program u i o -> [i Identity] -> m (i Array)
load (Program p) is = do
  let is' = DSL.getApVars (traverse (DSL.ApVars . DSL.mapVars (const ((:[]) . runIdentity))) is)
  DSL.forVars is' (\ s vs -> runLiftIO $ do
    b <- gen1 @(B.Buffer 'B.Array _)
    a <- gen1
    loc <- C.withCString s (checkingGLError . glGetAttribLocation p)
    a <$ unless (loc < 0) (do
      bind (Just b)
      B.realloc b (length vs) B.Static B.Draw
      B.copy b 0 vs

      bind (Just a)
      checkingGLError $ glEnableVertexAttribArray (fromIntegral loc)
      checkingGLError $ glVertexAttribPointer (fromIntegral loc) (GL.glDims a) (GL.glType a) GL_FALSE 0 nullPtr))
