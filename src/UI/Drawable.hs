{-# LANGUAGE NamedFieldPuns #-}
module UI.Drawable
( Drawable(..)
, using
) where

import Control.Effect.Lift
import Control.Effect.Reader
import Data.Functor.I
import GL.Array
import GL.Program

data Drawable u v o = Drawable
  { program :: Program u v o
  , array   :: Array (v I)
  }

using
  :: ( Has (Lift IO) sig m
     , Has (Reader a) sig m
     )
  => (a -> Drawable u v o)
  -> ArrayT v (ProgramT u v o m) b
  -> m b
using getDrawable m = do
  Drawable { program, array } <- asks getDrawable
  use program $ bindArray array m
