{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
module GL.Array
( Array(..)
, configureArray
, Mode(..)
, drawArrays
, drawArraysInstanced
, load
, bindArray
, ArrayT(..)
, HasArray(..)
) where

import           Control.Algebra
import           Control.Carrier.Reader
import           Control.Effect.Finally
import           Control.Monad.IO.Class.Lift
import           Control.Monad.Trans.Class
import           Data.Coerce
import           Data.Functor.I
import           Data.Interval
import           Foreign.Ptr
import qualified Foreign.Storable as S
import           GHC.Stack
import qualified GL.Buffer as B
import           GL.Enum as GL
import           GL.Error
import           GL.Object
import           GL.Program (HasProgram(..), ProgramT(..))
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


configureArray :: (DSL.Vars i, S.Storable (i I), Has (Lift IO) sig m) => B.Buffer 'B.Array (i I) -> Array (i I) -> m ()
configureArray _ a = DSL.foldVarsM (\ f@DSL.Field { DSL.location, DSL.offset } _ -> runLiftIO $ do
  checkingGLError $ glEnableVertexAttribArray (fromIntegral location)
  checkingGLError $ glVertexAttribPointer     (fromIntegral location) (GL.glDims f) (GL.glType f) GL_FALSE (fromIntegral (S.sizeOf (elemA a))) (nullPtr `plusPtr` DSL.getOffset offset)) (DSL.makeVars id `like` a) where
  like :: a DSL.Field -> Array (a c) -> a DSL.Field
  like = const
  elemA :: Array (i I) -> i I
  elemA _ = undefined


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
     , HasArray i m
     , HasProgram u i o m
     )
  => Mode
  -> Interval Int
  -> m ()
drawArrays mode i = askProgram >> askArray >> checkingGLError (runLiftIO (glDrawArrays (glEnum mode) (fromIntegral (min_ i)) (fromIntegral (size i))))

drawArraysInstanced
  :: ( Has (Lift IO) sig m
     , HasCallStack
     , HasArray i m
     , HasProgram u i o m
     )
  => Mode
  -> Interval Int
  -> Int
  -> m ()
drawArraysInstanced mode i n = askProgram >> askArray >> checkingGLError (runLiftIO (glDrawArraysInstanced (glEnum mode) (fromIntegral (min_ i)) (fromIntegral (size i)) (fromIntegral n)))


load :: (DSL.Vars i, S.Storable (i I), Has Finally sig m, Has (Lift IO) sig m) => [i I] -> m (Array (i I))
load is = do
  b <- gen1 @(B.Buffer 'B.Array _)
  a <- gen1
  bind (Just b)
  bind (Just a)
  B.realloc b (length is) B.Static B.Draw
  B.copy b 0 is

  a <$ configureArray b a


bindArray :: Has (Lift IO) sig m => Array (i I) -> ArrayT i m a -> m a
bindArray array (ArrayT m) = do
  bind (Just array)
  a <- runReader array m
  a <$ bind @(Array _) Nothing

class Monad m => HasArray i m | m -> i where
  askArray :: m (Array (i I))


newtype ArrayT i m a = ArrayT { runArrayT :: ReaderC (Array (i I)) m a }
  deriving (Applicative, Functor, Monad, MonadIO, MonadTrans)

deriving instance HasArray     i   m => HasArray     i   (ProgramT u i o m)
deriving instance HasProgram u i o m => HasProgram u i o (ArrayT     i   m)

instance HasArray i m => HasArray i (ReaderC r m) where
  askArray = lift askArray

instance Algebra sig m => Algebra sig (ArrayT i m) where
  alg = ArrayT . send . handleCoercible

instance Algebra sig m => HasArray i (ArrayT i m) where
  askArray = ArrayT ask
