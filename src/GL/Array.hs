{-# LANGUAGE DataKinds, FlexibleContexts, GeneralizedNewtypeDeriving, LambdaCase, ScopedTypeVariables, TypeApplications #-}
module GL.Array
( Array(..)
, configureArray
, Mode(..)
, drawArrays
, loadVertices
) where

import Control.Effect.Finally
import Control.Monad.IO.Class.Lift
import Data.Coerce
import Data.Interval
import Data.Proxy
import Foreign.Ptr
import qualified Foreign.Storable as S
import GHC.Stack
import GHC.TypeLits
import qualified GL.Buffer as GL
import GL.Error
import GL.Object
import GL.Scalar
import Graphics.GL.Core41
import Graphics.GL.Types
import Linear.V

newtype Array n = Array { unArray :: GLuint }
  deriving (S.Storable)

instance Object (Array n) where
  gen n = runLiftIO . glGenVertexArrays n . coerce
  delete n = runLiftIO . glDeleteVertexArrays n . coerce

instance Bind (Array n) where
  bind = checkingGLError . runLiftIO . glBindVertexArray . maybe 0 unArray


configureArray :: forall v n m sig . (KnownNat (Size v), Scalar n, Has (Lift IO) sig m) => GL.Buffer 'GL.Array (v n) -> Array (v n) -> m ()
configureArray _ _ = runLiftIO $ do
  glEnableVertexAttribArray 0
  glVertexAttribPointer 0 (fromIntegral (natVal (Proxy @(Size v)))) (glType (Proxy @n)) GL_FALSE 0 nullPtr


data Mode
  = Points
  | Lines
  | LineStrip
  | LineLoop
  | TriangleStrip
  | Triangles
  deriving (Eq, Show)

modeToGLEnum :: Mode -> GLenum
modeToGLEnum = \case
  Points        -> GL_POINTS
  Lines         -> GL_LINES
  LineStrip     -> GL_LINE_STRIP
  LineLoop      -> GL_LINE_LOOP
  TriangleStrip -> GL_TRIANGLE_STRIP
  Triangles     -> GL_TRIANGLES


drawArrays :: (Has (Lift IO) sig m, HasCallStack) => Mode -> Interval Int -> m ()
drawArrays mode i = checkingGLError . runLiftIO $ glDrawArrays (modeToGLEnum mode) (fromIntegral (min_ i)) (fromIntegral (size i))


loadVertices :: (KnownNat (Size v), S.Storable (v n), Scalar n, Has Finally sig m, Has (Lift IO) sig m) => [v n] -> m (Array (v n))
loadVertices vertices = do
  buffer <- gen1
  array  <- gen1

  bind (Just buffer)
  GL.realloc buffer (length vertices) GL.Static GL.Draw
  GL.copy buffer 0 vertices

  bind (Just array)
  array <$ configureArray buffer array
