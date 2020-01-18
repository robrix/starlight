{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module GL.Array
( Array(..)
, configureInterleaved
, configureSeparate
, Type(..)
, drawArrays
, multiDrawArrays
, drawArraysInstanced
, drawElements
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


configureInterleaved :: forall v m sig . (Effect sig, HasArray v m, B.HasBuffer 'B.Array (v I) m, Vars v, Has Check sig m, Has (Lift IO) sig m, Has Trace sig m) => m ()
configureInterleaved = askArray >> B.askBuffer @'B.Array >> evalState (Offset 0) (configureVars @v (S.sizeOf @(Fields v) undefined) (defaultVars @v))

configureSeparate :: forall v1 v2 m sig . (Effect sig, HasArray (v1 :**: v2) m, Vars v1, Vars v2, Has Check sig m, Has (Lift IO) sig m, Has Trace sig m) => B.Buffer 'B.Array (v1 I) -> B.Buffer 'B.Array (v2 I) -> m ()
configureSeparate b1 b2 = evalState (Offset 0) (askArray >> B.bindBuffer b1 (configureVars @v1 stride (defaultVars @v1)) >> B.bindBuffer b2 (configureVars @v2 stride (defaultVars @v2))) where
  stride = S.sizeOf @((v1 :**: v2) I) undefined

configureVars :: (Vars v, Has Check sig m, Has (Lift IO) sig m, Has (State Offset) sig m, Has Trace sig m) => Int -> v Maybe -> m ()
configureVars stride = foldVarsM (\ (Field{ location, name } :: Field Maybe a) -> runLiftIO $ do
  offset <- get
  let size   = S.sizeOf @a undefined
      K ty   = GL.glType @a
      K dims = GL.glDims @a
  put (offset <> Offset size)

  trace $ "configuring field " <> name <> " attrib " <> show location <> " at offset " <> show offset <> " stride " <> show stride <> " dims " <> show dims <> " type " <> show ty

  checking $ glEnableVertexAttribArray (fromIntegral location)
  checking $ glVertexAttribPointer     (fromIntegral location) dims ty GL_FALSE (fromIntegral stride) (nullPtr `plusPtr` getOffset offset))


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

drawElements
  :: ( Has Check sig m
     , Has (Lift IO) sig m
     , HasArray v m
     , B.HasBuffer 'B.ElementArray Word32 m
     , HasCallStack
     , HasProgram u v o m
     )
  => Type
  -> Interval I Int
  -> m ()
drawElements mode i = do
  _ <- askProgram
  _ <- askArray
  _ <- B.askBuffer @'B.ElementArray
  checking (runLiftIO (glDrawElements (glEnum mode) (fromIntegral (size i)) GL_UNSIGNED_INT (nullPtr `plusPtr` (getI (min' i) * S.sizeOf @Word32 0))))

drawElementsInstanced
  :: ( Has Check sig m
     , Has (Lift IO) sig m
     , HasArray v m
     , B.HasBuffer 'B.ElementArray Word32 m
     , HasCallStack
     , HasProgram u v o m
     )
  => Type
  -> Interval I Int
  -> Int
  -> m ()
drawElementsInstanced mode i n = do
  _ <- askProgram
  _ <- askArray
  _ <- B.askBuffer @'B.ElementArray
  checking (runLiftIO (glDrawElementsInstanced (glEnum mode) (fromIntegral (size i)) GL_UNSIGNED_INT (nullPtr `plusPtr` (getI (min' i) * S.sizeOf @Word32 0)) (fromIntegral n)))


load :: (Effect sig, Vars v, S.Storable (v I), Has Check sig m, Has Finally sig m, Has (Lift IO) sig m, Has Trace sig m) => [v I] -> m (B.Buffer 'B.Array (v I), Array (v I))
load is = do
  b <- gen1 @(B.Buffer 'B.Array _)
  a <- gen1
  bindArray a . B.bindBuffer b $ do
    B.realloc @'B.Array (length is) B.Static B.Draw
    B.copy @'B.Array 0 is

    (b, a) <$ configureInterleaved


bindArray :: (Has Check sig m, Has (Lift IO) sig m) => Array (v I) -> ArrayC v m a -> m a
bindArray array (ArrayC m) = do
  bind (Just array)
  a <- runReader array m
  a <$ bind @(Array _) Nothing

class Monad m => HasArray v m | m -> v where
  askArray :: m (Array (v I))


newtype ArrayC v m a = ArrayC { runArrayT :: ReaderC (Array (v I)) m a }
  deriving (Applicative, Functor, Monad, MonadIO, MonadTrans)

deriving instance HasArray     v   m => HasArray     v   (ProgramC u v o m)
deriving instance HasArray     v   m => HasArray     v   (B.BufferC ty x m)
deriving instance B.HasBuffer ty x m => B.HasBuffer ty x (ArrayC     v   m)
deriving instance HasProgram u v o m => HasProgram u v o (ArrayC     v   m)

instance HasArray v m => HasArray v (ReaderC r m) where
  askArray = lift askArray

instance HasArray v m => HasArray v (StateC s m) where
  askArray = lift askArray

instance Algebra sig m => Algebra sig (ArrayC v m) where
  alg = ArrayC . send . handleCoercible

instance Algebra sig m => HasArray v (ArrayC v m) where
  askArray = ArrayC ask
