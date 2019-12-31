module UI.Window
( swap
, poll
, input
, size
, scale
, runWindow
, Window
) where

import           Control.Carrier.Lift
import           Control.Carrier.Reader
import qualified Control.Concurrent.Lift as CC
import qualified Control.Exception.Lift as E
import           Control.Monad ((<=<))
import           Control.Monad.IO.Class.Lift
import           Data.Text (Text)
import           Graphics.GL.Core41
import           Linear.V2 as Linear
import           Linear.V4 as Linear
import           SDL

swap :: (Has (Lift IO) sig m, Has (Reader Window) sig m) => m ()
swap = ask >>= runLiftIO . glSwapWindow

poll :: Has (Lift IO) sig m => m (Maybe Event)
poll = runLiftIO pollEvent

input :: Has (Lift IO) sig m => (Event -> m ()) -> m ()
input h = go where
  go = poll >>= maybe (pure ()) (const go <=< h)

size :: (Num a, Has (Lift IO) sig m, Has (Reader Window) sig m) => m (V2 a)
size = do
  size <- ask >>= runLiftIO . get . windowSize
  pure (fromIntegral <$> size)

scale :: (Num a, Has (Lift IO) sig m, Has (Reader Window) sig m) => m a
scale = do
  config <- ask >>= runLiftIO . getWindowConfig
  pure $! if windowHighDPI config then 2 else 1


runWindow :: Has (Lift IO) sig m => Text -> V2 Int -> ReaderC Window m a -> m a
runWindow name size m = withSDL $
  withSDLWindow name size $ \ window ->
    withGLContext window $ \ _ ->
      runReader window m


withSDL :: Has (Lift IO) sig m => m a -> m a
withSDL = CC.runInBoundThread . E.bracket_ (runLiftIO initializeAll) (runLiftIO (glFinish >> quit))

withSDLWindow :: Has (Lift IO) sig m => Text -> Linear.V2 Int -> (Window -> m a) -> m a
withSDLWindow name size = E.bracket
  (runLiftIO (createWindow name windowConfig))
  (runLiftIO . destroyWindow) where
  windowConfig = defaultWindow
    { windowInitialSize = fromIntegral <$> size
    , windowResizable = True
    , windowPosition = Centered
    , windowGraphicsContext = OpenGLContext glConfig
    , windowHighDPI = True
    }
  glConfig = defaultOpenGL
    { glProfile = Core Normal 4 1
    , glColorPrecision = V4 8 8 8 8
    }

withGLContext :: Has (Lift IO) sig m => Window -> (GLContext -> m a) -> m a
withGLContext window = E.bracket (runLiftIO (glCreateContext window)) (runLiftIO . glDeleteContext)
