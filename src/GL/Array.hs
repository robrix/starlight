{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
module GL.Array
( Array(..)
, configureArray
, Mode(..)
, drawArrays
, loadVertices
) where

import           Control.Algebra
import           Control.Effect.Finally
import           Control.Monad.IO.Class.Lift
import           Data.Coerce
import           Data.Interval
import           Foreign.Ptr
import qualified Foreign.Storable as S
import           GHC.Stack
import qualified GL.Buffer as B
import           GL.Enum as GL
import           GL.Error
import           GL.Object
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
