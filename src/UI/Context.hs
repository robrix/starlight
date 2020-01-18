{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module UI.Context
( Context
, Pixels(..)
, runContext
) where

import           Control.Carrier.Reader
import           Control.Effect.Lift
import qualified Control.Exception.Lift as E
import           Control.Monad.IO.Class.Lift
import           Data.Functor.I
import           Foreign.Storable
import           GL.Type as GL
import           GL.Uniform
import           Graphics.GL.Core41
import           SDL

type Context = GLContext

newtype Pixels a = Pixels { getPixels :: a }
  deriving (Column, Conjugate, Enum, Epsilon, Eq, Foldable, Floating, Fractional, Functor, Integral, Num, Ord, Real, RealFloat, RealFrac, Row, Show, Storable, Traversable, GL.Type, Uniform)
  deriving (Additive, Applicative, Metric, Monad) via I

runContext :: (Has (Lift IO) sig m, Has (Reader Window) sig m) => ReaderC Context m a -> m a
runContext = E.bracket
  (ask >>= runLiftIO . glCreateContext)
  (\ c -> runLiftIO (glFinish >> glDeleteContext c))
  . flip runReader
