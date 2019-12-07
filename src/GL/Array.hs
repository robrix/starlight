{-# LANGUAGE DataKinds, GeneralizedNewtypeDeriving, LambdaCase, ScopedTypeVariables, TypeApplications #-}
module GL.Array
( Array(..)
, withArray
, Mode(..)
, Range(..)
, drawArrays
) where

import Control.Monad.IO.Class.Lift
import Data.Coerce
import Data.Proxy
import Foreign.Ptr
import qualified Foreign.Storable as S
import qualified GL.Buffer as GL
import GL.Error
import GL.Object
import GL.Scalar
import Graphics.GL.Core41
import Graphics.GL.Types

newtype Array n = Array { unArray :: GLuint }
  deriving (S.Storable)

withArray :: forall v n m a sig . (Foldable v, Scalar n, Has (Lift IO) sig m) => [v n] -> (Array n -> m a) -> m a
withArray vertices body = with $ \ buffer -> runLiftIO $ do
  GL.realloc buffer vertices GL.Static GL.Draw
  with $ \ array -> bind array . bind @(GL.Buffer 'GL.Array n) buffer $ do
    glEnableVertexAttribArray 0
    glVertexAttribPointer 0 (fromIntegral (length (head vertices))) (glType (Proxy @n)) GL_FALSE 0 nullPtr
    LiftIO (body array)

instance Object (Array n) where
  gen n = glGenVertexArrays n . coerce
  delete n = glDeleteVertexArrays n . coerce

instance Bind (Array n) where
  nullObject = Array 0
  bindObject = checkingGLError . runLiftIO . glBindVertexArray . unArray


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

data Range = Range
  { rangeFrom  :: {-# UNPACK #-} !Int
  , rangeCount :: {-# UNPACK #-} !Int
  }
  deriving (Eq, Show)

drawArrays :: Has (Lift IO) sig m => Mode -> Range -> m ()
drawArrays mode (Range from count) = checkingGLError . runLiftIO $ glDrawArrays (modeToGLEnum mode) (fromIntegral from) (fromIntegral count)
