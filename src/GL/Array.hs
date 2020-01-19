{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
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
, HasArray
, askArray
, ArrayC
) where

import           Control.Carrier.Fresh.Strict
import           Control.Carrier.Reader
import           Control.Carrier.State.Strict
import           Control.Effect.Finally
import           Control.Effect.Labelled
import           Control.Effect.Trace
import           Control.Monad.IO.Class.Lift
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
import           GL.Program (HasProgram, askProgram)
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


configureInterleaved :: forall v m sig . (Effect sig, HasArray v sig m, B.HasBuffer 'B.Array (v I) sig m, Vars v, Has Check sig m, Has (Lift IO) sig m, Has Trace sig m) => m ()
configureInterleaved = askArray >> B.askBuffer @'B.Array >> evalState (Offset 0) (evalFresh 0 (configureVars @v (S.sizeOf @(Fields v) undefined) (defaultVars @v)))

configureSeparate :: forall v1 v2 m sig . (Effect sig, HasArray (v1 :**: v2) sig m, Vars v1, Vars v2, Has Check sig m, Has (Lift IO) sig m, Has Trace sig m) => B.Buffer 'B.Array (v1 I) -> B.Buffer 'B.Array (v2 I) -> m ()
configureSeparate b1 b2 = evalState (Offset 0) (evalFresh 0 (askArray >> B.bindBuffer b1 (configureVars @v1 stride (defaultVars @v1)) >> B.bindBuffer b2 (configureVars @v2 stride (defaultVars @v2)))) where
  stride = S.sizeOf @((v1 :**: v2) I) undefined

configureVars :: (Vars v, Has Check sig m, Has Fresh sig m, Has (Lift IO) sig m, Has (State Offset) sig m, Has Trace sig m) => Int -> v Proxy -> m ()
configureVars stride = foldVarsM' (\ (Field{ location, name } :: Field Proxy a) -> runLiftIO $ do
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
     , HasArray v sig m
     , HasCallStack
     , HasProgram u v o sig m
     )
  => Type
  -> Interval I Int
  -> m ()
drawArrays mode i = askProgram >> askArray >> checking (runLiftIO (glDrawArrays (glEnum mode) (fromIntegral (min' i)) (fromIntegral (size i))))

multiDrawArrays
  :: ( Has Check sig m
     , Has (Lift IO) sig m
     , HasArray v sig m
     , HasCallStack
     , HasProgram u v o sig m
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
     , HasArray v sig m
     , HasCallStack
     , HasProgram u v o sig m
     )
  => Type
  -> Interval I Int
  -> Int
  -> m ()
drawArraysInstanced mode i n = askProgram >> askArray >> checking (runLiftIO (glDrawArraysInstanced (glEnum mode) (fromIntegral (min' i)) (fromIntegral (size i)) (fromIntegral n)))

drawElements
  :: ( Has Check sig m
     , Has (Lift IO) sig m
     , HasArray v sig m
     , B.HasBuffer 'B.ElementArray Word32 sig m
     , HasCallStack
     , HasProgram u v o sig m
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
     , HasArray v sig m
     , B.HasBuffer 'B.ElementArray Word32 sig m
     , HasCallStack
     , HasProgram u v o sig m
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
bindArray array m = do
  bind (Just array)
  a <- runReader array (runDep m)
  a <$ bind @(Array _) Nothing

type HasArray v sig m = DHas Array (Reader (Array (v I))) sig m

askArray :: DHas Array (Reader (Array (v I))) sig m => m (Array (v I))
askArray = runInDep @_ @Array ask

type ArrayC v = Dep Array (ReaderC (Array (v I)))
