{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
module GL.Array
( Array(..)
, configureArray
, Type(..)
, drawArrays
, multiDrawArrays
, drawArraysInstanced
, drawElementsInstanced
, load
, bindArray
, HasArray(..)
, ArrayC(..)
) where

import           Control.Algebra
import           Control.Carrier.Reader
import           Control.Carrier.State.Strict
import           Control.Effect.Finally
import           Control.Effect.Trace
import           Control.Monad.IO.Class.Lift
import           Control.Monad.Trans.Class
import           Data.Functor.I
import           Data.Functor.K
import           Data.Functor.Interval
import           Data.List (genericLength)
import           Data.Word (Word32)
import           Foreign.Marshal.Array.Lift
import           Foreign.Ptr
import qualified Foreign.Storable as S
import           GHC.Stack
import qualified GL.Buffer as B
import           GL.Effect.Check
import           GL.Enum as GL
import           GL.Object
import           GL.Primitive
import           GL.Program (HasProgram(..), ProgramC(..))
import           GL.Shader.Vars
import qualified GL.Type as GL
import           Graphics.GL.Core41
import           Graphics.GL.Types

newtype Array n = Array { unArray :: GLuint }

instance Object (Array n) where
  gen = defaultGenWith glGenVertexArrays Array
  delete = defaultDeleteWith glDeleteVertexArrays unArray

instance Bind (Array n) where
  bind = checking . runLiftIO . glBindVertexArray . maybe 0 unArray


configureArray :: (Effect sig, HasArray v m, B.HasBuffer 'B.Array (v I) m, Vars v, S.Storable (v I), Has Check sig m, Has (Lift IO) sig m, Has Trace sig m) => m ()
configureArray = do
  a <- askArray <* B.askBuffer @'B.Array
  evalState (Offset 0) $ foldVarsM (\ f@Field { location, name } -> runLiftIO $ do
    offset <- get
    let size = S.sizeOf (undefinedAtFieldType f)
        stride = S.sizeOf (elemA a)
        K ty = typeFor f
        K dims = dimsFor f
    put (offset <> Offset size)

    trace $ "configuring field " <> name <> " attrib " <> show location <> " at offset " <> show offset <> " stride " <> show stride <> " dims " <> show dims <> " type " <> show ty

    checking $ glEnableVertexAttribArray (fromIntegral location)
    checking $ glVertexAttribPointer     (fromIntegral location) dims ty GL_FALSE (fromIntegral stride) (nullPtr `plusPtr` getOffset offset)) (defaultVars `like` a) where
  typeFor :: GL.Type a => Field v a -> K GLenum a
  typeFor _ = GL.glType
  dimsFor :: GL.Type a => Field v a -> K GLint a
  dimsFor _ = GL.glDims
  like :: v Maybe -> Array (v I) -> v Maybe
  like = const
  elemA :: Array (v I) -> v I
  elemA _ = undefined
  undefinedAtFieldType :: Field v a -> a
  undefinedAtFieldType _ = undefined


drawArrays
  :: ( Has Check sig m
     , Has (Lift IO) sig m
     , HasArray v m
     , HasCallStack
     , HasProgram u v o m
     )
  => Type
  -> Interval I Int
  -> m ()
drawArrays mode i = askProgram >> askArray >> checking (runLiftIO (glDrawArrays (glEnum mode) (fromIntegral (min' i)) (fromIntegral (size i))))

multiDrawArrays
  :: ( Has Check sig m
     , Has (Lift IO) sig m
     , HasArray v m
     , HasCallStack
     , HasProgram u v o m
     )
  => Type
  -> [Interval I Int]
  -> m ()
multiDrawArrays mode is
  | null is   = pure ()
  | otherwise = do
    _ <- askProgram
    _ <- askArray
    withArray (map (fromIntegral . min') is) $ \ firsts -> withArray (map (fromIntegral . size) is) $ \ counts ->
      checking (runLiftIO (glMultiDrawArrays (glEnum mode) firsts counts (fromIntegral (length is))))

drawArraysInstanced
  :: ( Has Check sig m
     , Has (Lift IO) sig m
     , HasArray v m
     , HasCallStack
     , HasProgram u v o m
     )
  => Type
  -> Interval I Int
  -> Int
  -> m ()
drawArraysInstanced mode i n = askProgram >> askArray >> checking (runLiftIO (glDrawArraysInstanced (glEnum mode) (fromIntegral (min' i)) (fromIntegral (size i)) (fromIntegral n)))

drawElementsInstanced
  :: ( Has Check sig m
     , Has (Lift IO) sig m
     , HasArray v m
     , HasCallStack
     , HasProgram u v o m
     )
  => Type
  -> [Int]
  -> Int
  -> m ()
drawElementsInstanced mode i n = askProgram >> askArray >> checking (runLiftIO (withArray (map (fromIntegral @_ @Word32) i) (\ p -> glDrawElementsInstanced (glEnum mode) (genericLength i) GL_UNSIGNED_INT (castPtr p) (fromIntegral n))))


load :: (Effect sig, Vars v, S.Storable (v I), Has Check sig m, Has Finally sig m, Has (Lift IO) sig m, Has Trace sig m) => [v I] -> m (B.Buffer 'B.Array (v I), Array (v I))
load is = do
  b <- gen1 @(B.Buffer 'B.Array _)
  a <- gen1
  bindArray a . B.bindBuffer b $ do
    B.realloc @'B.Array (length is) B.Static B.Draw
    B.copy @'B.Array 0 is

    (b, a) <$ configureArray


bindArray :: (Has Check sig m, Has (Lift IO) sig m) => Array (v I) -> ArrayC v m a -> m a
bindArray array (ArrayC m) = do
  bind (Just array)
  a <- runReader array m
  a <$ bind @(Array _) Nothing

class Monad m => HasArray i m | m -> i where
  askArray :: m (Array (i I))


newtype ArrayC v m a = ArrayC { runArrayT :: ReaderC (Array (v I)) m a }
  deriving (Applicative, Functor, Monad, MonadIO, MonadTrans)

deriving instance HasArray              v      m => HasArray              v      (ProgramC u    v    o m)
deriving instance HasArray              v      m => HasArray              v      (B.BufferC ty (v I)   m)
deriving instance B.HasBuffer 'B.Array (v I)   m => B.HasBuffer 'B.Array (v I)   (ArrayC        v      m)
deriving instance HasProgram u          v    o m => HasProgram u          v    o (ArrayC        v      m)

instance HasArray v m => HasArray v (ReaderC r m) where
  askArray = lift askArray

instance Algebra sig m => Algebra sig (ArrayC v m) where
  alg = ArrayC . send . handleCoercible

instance Algebra sig m => HasArray v (ArrayC v m) where
  askArray = ArrayC ask
