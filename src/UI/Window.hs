module UI.Window
( withSDL
, withSDLWindow
, withGLContext
, Window
) where

import           Control.Carrier.Lift
import qualified Control.Concurrent.Lift as CC
import qualified Control.Exception.Lift as E
import           Control.Monad.IO.Class.Lift
import           Data.Text (Text)
import           Graphics.GL.Core41
import           Linear.V2 as Linear
import           Linear.V4 as Linear
import           SDL.Init
import           SDL.Video

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
