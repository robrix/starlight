{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
module UI.Window
( Pixels(..)
, swap
, poll
, input
, size
, ratio
, runSDL
, runWindow
, Window
) where

import           Control.Carrier.Lift
import           Control.Carrier.Reader
import qualified Control.Concurrent.Lift as CC
import qualified Control.Exception.Lift as E
import           Control.Lens ((^.))
import           Control.Monad ((<=<))
import           Control.Monad.IO.Class.Lift
import           Data.Fixed (div')
import           Data.Functor.I
import           Data.Functor.K
import           Data.Text (Text)
import           Foreign.Storable
import           GL.Type as GL
import           GL.Uniform
import           Linear.V2 as Linear
import           Linear.V4 as Linear
import           SDL
import           Unit.Length

newtype Pixels a = Pixels { getPixels :: a }
  deriving (Column, Conjugate, Enum, Epsilon, Eq, Foldable, Floating, Fractional, Functor, Integral, Num, Ord, Real, RealFloat, RealFrac, Row, Show, Storable, Traversable, GL.Type, Uniform)
  deriving (Additive, Applicative, Metric, Monad) via I

instance Unit Pixels where
  type Dim Pixels = Length
  suffix = K ("px"++)


swap :: (Has (Lift IO) sig m, Has (Reader Window) sig m) => m ()
swap = ask >>= runLiftIO . glSwapWindow

poll :: Has (Lift IO) sig m => m (Maybe Event)
poll = runLiftIO pollEvent

input :: Has (Lift IO) sig m => (Event -> m ()) -> m ()
input h = go where
  go = poll >>= maybe (pure ()) (const go <=< h)

size :: (Num a, Has (Lift IO) sig m, Has (Reader Window) sig m) => m (V2 (Pixels a))
size = do
  size <- ask >>= runLiftIO . get . windowSize
  pure (fromIntegral <$> size)

ratio :: (Integral a, Has (Lift IO) sig m, Has (Reader Window) sig m) => m a
ratio = runLiftIO $ do
  window <- ask
  drawableSize <- glGetDrawableSize window
  windowSize <- get (windowSize window)
  pure $! (drawableSize^._y) `div'` (windowSize^._y)


runSDL :: Has (Lift IO) sig m => m a -> m a
runSDL = CC.runInBoundThread . E.bracket_ (runLiftIO initializeAll) (runLiftIO quit)

runWindow :: Has (Lift IO) sig m => Text -> V2 (Pixels Int) -> ReaderC Window m a -> m a
runWindow name size = E.bracket
  (runLiftIO (createWindow name windowConfig))
  (runLiftIO . destroyWindow)
  . flip runReader where
  windowConfig = defaultWindow
    { windowInitialSize     = fromIntegral <$> size
    , windowResizable       = True
    , windowPosition        = Centered
    , windowGraphicsContext = OpenGLContext glConfig
    , windowHighDPI         = True
    , windowMode            = FullscreenDesktop
    }
  glConfig = defaultOpenGL
    { glProfile        = Core Normal 4 1
    , glColorPrecision = V4 8 8 8 8
    }
