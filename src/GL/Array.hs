{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
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
, HasArray(..)
, ArrayC(..)
) where

import           Control.Algebra
import           Control.Carrier.Reader
import           Control.Carrier.State.Strict
import           Control.Effect.Finally
import           Control.Monad.IO.Class.Lift
import           Control.Monad.Trans.Class
import           Data.Coerce
import           Data.Functor.Identity
import           Data.Functor.Interval
import           Foreign.Ptr
import qualified Foreign.Storable as S
import           GHC.Stack
import qualified GL.Buffer as B
import           GL.Effect.Check
import           GL.Enum as GL
import           GL.Object
import           GL.Program (HasProgram(..), ProgramC(..))
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
  bind = checking . runLiftIO . glBindVertexArray . maybe 0 unArray


configureArray :: (Effect sig, HasArray i m, B.HasBuffer 'B.Array i m, DSL.Vars i, S.Storable (i Identity), Has Check sig m, Has (Lift IO) sig m) => m ()
configureArray = do
  a <- askArray <* B.askBuffer
  evalState (DSL.Offset 0) $ DSL.foldVarsM (\ f@DSL.Field { DSL.location } -> runLiftIO $ do
    o <- get
    let size = S.sizeOf (elemA a)
        offset = o <> DSL.Offset size
    put offset
    checking $ glEnableVertexAttribArray (fromIntegral location)
    checking $ glVertexAttribPointer     (fromIntegral location) (GL.glDims f) (GL.glType f) GL_FALSE (fromIntegral size) (nullPtr `plusPtr` DSL.getOffset o)) (DSL.defaultVars `like` a) where
  like :: a Maybe -> Array (a Identity) -> a Maybe
  like = const
  elemA :: Array (i Identity) -> i Identity
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
  :: ( Has Check sig m
     , Has (Lift IO) sig m
     , HasArray i m
     , HasCallStack
     , HasProgram u i o m
     )
  => Mode
  -> Interval Identity Int
  -> m ()
drawArrays mode i = askProgram >> askArray >> checking (runLiftIO (glDrawArrays (glEnum mode) (fromIntegral (min' i)) (fromIntegral (size i))))

drawArraysInstanced
  :: ( Has Check sig m
     , Has (Lift IO) sig m
     , HasArray i m
     , HasCallStack
     , HasProgram u i o m
     )
  => Mode
  -> Interval Identity Int
  -> Int
  -> m ()
drawArraysInstanced mode i n = askProgram >> askArray >> checking (runLiftIO (glDrawArraysInstanced (glEnum mode) (fromIntegral (min' i)) (fromIntegral (size i)) (fromIntegral n)))


load :: (Effect sig, DSL.Vars i, S.Storable (i Identity), Has Check sig m, Has Finally sig m, Has (Lift IO) sig m) => [i Identity] -> m (B.Buffer 'B.Array (i Identity), Array (i Identity))
load is = do
  b <- gen1 @(B.Buffer 'B.Array _)
  a <- gen1
  bindArray a . B.bindBuffer b $ do
    B.realloc b (length is) B.Static B.Draw
    B.copy b 0 is

    (b, a) <$ configureArray


bindArray :: (Has Check sig m, Has (Lift IO) sig m) => Array (i Identity) -> ArrayC i m a -> m a
bindArray array (ArrayC m) = do
  bind (Just array)
  a <- runReader array m
  a <$ bind @(Array _) Nothing

class Monad m => HasArray i m | m -> i where
  askArray :: m (Array (i Identity))


newtype ArrayC i m a = ArrayC { runArrayT :: ReaderC (Array (i Identity)) m a }
  deriving (Applicative, Functor, Monad, MonadIO, MonadTrans)

deriving instance HasArray             i   m => HasArray             i   (ProgramC u   i o m)
deriving instance HasArray             i   m => HasArray             i   (B.BufferC ty i   m)
deriving instance B.HasBuffer 'B.Array i   m => B.HasBuffer 'B.Array i   (ArrayC       i   m)
deriving instance HasProgram u         i o m => HasProgram u         i o (ArrayC       i   m)

instance HasArray i m => HasArray i (ReaderC r m) where
  askArray = lift askArray

instance Algebra sig m => Algebra sig (ArrayC i m) where
  alg = ArrayC . send . handleCoercible

instance Algebra sig m => HasArray i (ArrayC i m) where
  askArray = ArrayC ask
