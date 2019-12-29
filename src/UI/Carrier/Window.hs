{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module UI.Carrier.Window
( -- * Window carrier
  runWindow
, WindowC(..)
  -- * Window effect
, module UI.Effect.Window
) where

import           Control.Algebra
import           Control.Carrier.Lift
import           Control.Carrier.Reader
import           Control.Monad.IO.Class.Lift
import           Data.Text (Text)
import           Linear.V2
import qualified SDL
import           UI.Effect.Window
import qualified UI.Window as UI

runWindow :: Has (Lift IO) sig m => Text -> V2 Int -> WindowC m a -> m a
runWindow name size (WindowC m) = UI.withSDL $
  UI.withSDLWindow name size $ \ window ->
    UI.withGLContext window $ \ _ ->
      runReader window m

newtype WindowC m a = WindowC (ReaderC UI.Window m a)
  deriving (Applicative, Functor, Monad, MonadFail, MonadIO)

instance (Has (Lift IO) sig m, Effect sig) => Algebra (Window :+: sig) (WindowC m) where
  alg = \case
    L (Swap   k) -> WindowC ask >>= runLiftIO . SDL.glSwapWindow >> k
    L (Poll   k) -> runLiftIO SDL.pollEvent >>= k
    L (Size   k) -> do
      window <- WindowC ask
      size <- sendIO (SDL.get (SDL.windowSize window))
      k (fromIntegral <$> size)
    L (Scale  k) -> do
      window <- WindowC ask
      config <- sendIO (SDL.getWindowConfig window)
      k $! if SDL.windowHighDPI config then 2 else 1
    R other      -> WindowC (send (handleCoercible other))
