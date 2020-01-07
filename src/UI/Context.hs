module UI.Context
( Context
, runContext
) where

import           Control.Carrier.Reader
import           Control.Effect.Lift
import qualified Control.Exception.Lift as E
import           Control.Monad.IO.Class.Lift
import           Graphics.GL.Core41
import           SDL

type Context = GLContext

runContext :: (Has (Lift IO) sig m, Has (Reader Window) sig m) => ReaderC Context m a -> m a
runContext = E.bracket
  (ask >>= runLiftIO . glCreateContext)
  (\ c -> runLiftIO (glFinish >> glDeleteContext c))
  . flip runReader
