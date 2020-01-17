{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
module UI.Drawable
( Drawable(..)
, using
, runDrawable
, loadingDrawable
) where

import           Control.Carrier.Reader
import           Control.Effect.Finally
import           Control.Effect.Lift
import           Control.Effect.Trace
import           Data.Functor.I
import           Foreign.Storable (Storable)
import           GL.Array
import qualified GL.Buffer as B
import           GL.Effect.Check
import           GL.Program
import           GL.Shader.DSL (Shader, Vars)

data Drawable u v o = Drawable
  { program :: Program u v o
  , array   :: Array (v I)
  , buffer  :: B.Buffer 'B.Array (v I)
  }

using
  :: ( Has Check sig m
     , Has (Lift IO) sig m
     , Has (Reader a) sig m
     , Vars u
     )
  => (a -> Drawable u v o)
  -> B.BufferC 'B.Array v (ArrayC v (ProgramC u v o m)) b
  -> m b
using getDrawable m = do
  Drawable { program, array, buffer } <- asks getDrawable
  use program $ bindArray array $ B.bindBuffer buffer m


runDrawable :: (Drawable u v o -> b) -> Drawable u v o -> ReaderC b m a -> m a
runDrawable makeDrawable = runReader . makeDrawable

loadingDrawable :: (Effect sig, Has Check sig m, Has Finally sig m, Has (Lift IO) sig m, Has Trace sig m, Storable (v I), Vars u, Vars v) => (Drawable u v o -> b) -> Shader u v o -> [v I] -> ReaderC b m a -> m a
loadingDrawable drawable shader vertices m = do
  program <- build shader
  (buffer, array) <- load vertices
  runDrawable drawable Drawable{ program, array, buffer } m
