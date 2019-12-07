{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module UI.Carrier.Window
( -- * Window carrier
  runWindow
, WindowC(..)
  -- * Window effect
, module UI.Effect.Window
) where

import Control.Carrier.Lift
import Control.Carrier.Reader
import Control.Monad.IO.Class
import Data.Text (Text)
import Linear.V2
import UI.Effect.Window
import qualified UI.Window as UI


runWindow :: Has (Lift IO) sig m => Text -> V2 Int -> WindowC m a -> m a
runWindow name size (WindowC m) = UI.withSDL $
  UI.withSDLWindow name size $ \ window ->
    UI.withGLContext window $ \ _ ->
      runReader window m

newtype WindowC m a = WindowC (ReaderC UI.Window m a)
  deriving (Applicative, Functor, Monad, MonadIO)
