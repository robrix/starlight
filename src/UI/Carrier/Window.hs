{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving, LambdaCase, MultiParamTypeClasses, TypeOperators, UndecidableInstances #-}
module UI.Carrier.Window
( -- * Window carrier
  runWindow
, WindowC(..)
  -- * Window effect
, module UI.Effect.Window
) where

import Control.Algebra
import Control.Carrier.Lift
import Control.Carrier.Reader
import Control.Monad.IO.Class.Lift
import Data.Function (fix)
import Data.Text (Text)
import Linear.V2
import qualified SDL as SDL
import UI.Effect.Window
import qualified UI.Window as UI

runWindow :: Has (Lift IO) sig m => Text -> V2 Int -> WindowC m a -> m a
runWindow name size (WindowC m) = UI.withSDL $
  UI.withSDLWindow name size $ \ window ->
    UI.withGLContext window $ \ _ ->
      runReader window m

newtype WindowC m a = WindowC (ReaderC UI.Window m a)
  deriving (Applicative, Functor, Monad, MonadIO)

instance Has (Lift IO) sig m => Algebra (Window :+: sig) (WindowC m) where
  alg = \case
    L (Draw m k) -> do
      window <- WindowC ask
      fix $ \ loop -> do
        a <- m
        events <- runLiftIO SDL.pollEvents
        if any ((== SDL.QuitEvent) . SDL.eventPayload) events then
          k a
        else
          runLiftIO (SDL.glSwapWindow window) >> loop
    L (Size   k) -> do
      window <- WindowC ask
      size <- sendIO (SDL.get (SDL.windowSize window))
      k (fromIntegral <$> size)
    L (Scale  k) -> do
      window <- WindowC ask
      config <- sendIO (SDL.getWindowConfig window)
      k $! if SDL.windowHighDPI config then 2 else 1
    R other      -> WindowC (send (handleCoercible other))
