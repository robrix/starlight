{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module UI.Window
( Coords(..)
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
import           Graphics.GL.Core41
import           Linear.V2 as Linear
import           Linear.V4 as Linear
import           SDL
import           System.Random (Random)
import           Unit.Length

-- FIXME: can we embed the ratio into this? maybe at the type level?
newtype Coords a = Coords { getCoords :: a }
  deriving (Column, Conjugate, Enum, Epsilon, Eq, Foldable, Floating, Fractional, Functor, Integral, Num, Ord, Random, Real, RealFloat, RealFrac, Row, Show, Storable, Traversable, GL.Type, Uniform)
  deriving (Additive, Applicative, Metric, Monad) via I

instance Unit Length Coords where
  suffix = K ('w':)


swap :: (Has (Lift IO) sig m, Has (Reader Window) sig m) => m ()
swap = runLiftIO glFlush >> ask >>= runLiftIO . glSwapWindow

poll :: Has (Lift IO) sig m => m (Maybe Event)
poll = runLiftIO pollEvent

input :: Has (Lift IO) sig m => (Event -> m ()) -> m ()
input h = go where
  go = poll >>= maybe (pure ()) (const go <=< h)

size :: (Num a, Has (Lift IO) sig m, Has (Reader Window) sig m) => m (V2 (Coords a))
size = do
  size <- asks windowSize >>= runLiftIO . get
  pure (fromIntegral <$> size)

ratio :: (Integral a, Has (Lift IO) sig m, Has (Reader Window) sig m) => m (I a)
ratio = runLiftIO $ do
  window <- ask
  drawableSize <- glGetDrawableSize window
  windowSize <- get (windowSize window)
  pure $! (drawableSize^._y) `div'` (windowSize^._y)


runSDL :: Has (Lift IO) sig m => m a -> m a
runSDL = CC.runInBoundThread . E.bracket_ (runLiftIO initializeAll) (runLiftIO quit)

runWindow :: Has (Lift IO) sig m => Text -> V2 (Coords Int) -> ReaderC Window m a -> m a
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
    , windowInputGrabbed    = True
    }
  glConfig = defaultOpenGL
    { glProfile        = Core Normal 4 1
    , glColorPrecision = V4 8 8 8 8
    }
