{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module GL.Array
( Array(..)
, configureArray
, Mode(..)
, drawArrays
, loadVertices
, useArray
, HasArray(..)
, ArrayT(..)
) where

import           Control.Algebra
import           Control.Carrier.Reader
import           Control.Effect.Finally
import           Control.Monad.IO.Class.Lift
import           Control.Monad.Trans.Class
import           Data.Coerce
import           Data.Functor.Identity
import           Data.Interval
import           Data.Proxy
import           Foreign.Ptr
import qualified Foreign.Storable as S
import           GHC.Stack
import           GHC.TypeLits
import qualified GL.Buffer as GL
import           GL.Enum as GL
import           GL.Error
import           GL.Object
import           GL.Scalar
import           Graphics.GL.Core41
import           Graphics.GL.Types
import           Linear.V

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

instance GL.Enum Mode where
  glEnum = \case
    Points        -> GL_POINTS
    Lines         -> GL_LINES
    LineStrip     -> GL_LINE_STRIP
    LineLoop      -> GL_LINE_LOOP
    TriangleStrip -> GL_TRIANGLE_STRIP
    Triangles     -> GL_TRIANGLES


drawArrays :: (Has (Lift IO) sig m, HasCallStack) => Mode -> Interval Int -> m ()
drawArrays mode i = checkingGLError . runLiftIO $ glDrawArrays (glEnum mode) (fromIntegral (min_ i)) (fromIntegral (size i))


loadVertices :: (KnownNat (Size v), S.Storable (v n), Scalar n, Has Finally sig m, Has (Lift IO) sig m) => [v n] -> m (Array (v n))
loadVertices vertices = do
  buffer <- gen1
  array  <- gen1

  bind (Just buffer)
  GL.realloc buffer (length vertices) GL.Static GL.Draw
  GL.copy buffer 0 vertices

  bind (Just array)
  array <$ configureArray buffer array


useArray :: forall i m a sig . (Has Finally sig m, Has (Lift IO) sig m) => Array (i Identity) -> ArrayT i m a -> m a
useArray p (ArrayT m) = do
  bind (Just p)
  a <- runReader p m
  a <$ bind @(Array (i Identity)) Nothing


class HasArray (i :: (* -> *) -> *) (m :: * -> *) | m -> i where
  askArray :: m (Array (i Identity))


newtype ArrayT (i :: (* -> *) -> *) m a = ArrayT { runArrayT :: ReaderC (Array (i Identity)) m a }
  deriving (Applicative, Functor, Monad, MonadIO, MonadTrans)

instance Algebra sig m => Algebra sig (ArrayT i m) where
  alg = ArrayT . send . handleCoercible

instance Algebra sig m => HasArray i (ArrayT i m) where
  askArray = ArrayT ask
