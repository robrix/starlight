{-# LANGUAGE DataKinds, FlexibleContexts, GeneralizedNewtypeDeriving, LambdaCase, ScopedTypeVariables, TypeApplications #-}
module GL.Array
( Array(..)
, withArray
, configureArray
, Mode(..)
, Range(..)
, drawArrays
) where

import Control.Monad.IO.Class.Lift
import Data.Coerce
import Data.Proxy
import Foreign.Ptr
import qualified Foreign.Storable as S
import GHC.Stack
import GHC.TypeLits
import qualified GL.Buffer as GL
import GL.Error
import GL.Object
import GL.Range
import GL.Scalar
import Graphics.GL.Core41
import Graphics.GL.Types
import Linear.V

newtype Array n = Array { unArray :: GLuint }
  deriving (S.Storable)

withArray :: forall v n m a sig . (KnownNat (Size v), S.Storable (v n), Scalar n, Has (Lift IO) sig m) => [v n] -> (Array (v n) -> m a) -> m a
withArray vertices body = with $ \ buffer -> runLiftIO . bind @(GL.Buffer 'GL.Array (v n)) buffer $ do
  GL.realloc buffer (length vertices) GL.Static GL.Draw
  GL.copy buffer 0 vertices
  with $ \ array -> bind array $ do
    glEnableVertexAttribArray 0
    glVertexAttribPointer 0 (fromIntegral (natVal (Proxy @(Size v)))) (glType (Proxy @n)) GL_FALSE 0 nullPtr
    LiftIO (body array)

instance Object (Array n) where
  gen n = glGenVertexArrays n . coerce
  delete n = glDeleteVertexArrays n . coerce

instance Bind (Array n) where
  nullObject = Array 0
  bindObject = checkingGLError . runLiftIO . glBindVertexArray . unArray


configureArray :: forall v n m sig . (KnownNat (Size v), Scalar n, Has (Lift IO) sig m) => GL.Buffer 'GL.Array (v n) -> Array (v n) -> m ()
configureArray buffer array = runLiftIO . bind buffer . bind array $ do
  glEnableVertexAttribArray 0
  glVertexAttribPointer 0 (fromIntegral (natVal (Proxy @(Size v)))) (glType (Proxy @n)) GL_FALSE 0 nullPtr


data Mode
  = Lines
  | LineStrip
  | LineLoop
  | TriangleStrip
  | Triangles
  deriving (Eq, Show)

modeToGLEnum :: Mode -> GLenum
modeToGLEnum = \case
  Lines         -> GL_LINES
  LineStrip     -> GL_LINE_STRIP
  LineLoop      -> GL_LINE_LOOP
  TriangleStrip -> GL_TRIANGLE_STRIP
  Triangles     -> GL_TRIANGLES


drawArrays :: (Has (Lift IO) sig m, HasCallStack) => Mode -> Range -> m ()
drawArrays mode (Range from count) = checkingGLError . runLiftIO $ glDrawArrays (modeToGLEnum mode) (fromIntegral from) (fromIntegral count)
