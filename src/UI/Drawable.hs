{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
module UI.Drawable
( Drawable(..)
, using
) where

import           Control.Effect.Lift
import           Control.Effect.Reader
import           Data.Functor.Identity
import           GL.Array
import qualified GL.Buffer as B
import           GL.Effect.Check
import           GL.Program
import           GL.Shader.DSL (Vars)

data Drawable u v o = Drawable
  { program :: Program u v o
  , array   :: Array (v Identity)
  , buffer  :: B.Buffer 'B.Array (v Identity)
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
