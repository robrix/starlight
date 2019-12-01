{-# LANGUAGE DeriveAnyClass #-}
module UI.Window where

import qualified Control.Concurrent as CC
import qualified Control.Exception as E
import Control.Monad
import Data.Bits
import Data.Foldable
import Data.Word
import qualified Foreign.C.String as C
import Foreign.Ptr
import Linear.V2 as Linear
import SDL.Event
import SDL.Init
import qualified SDL.Raw as SDL
import System.Exit

withWindow :: String -> Linear.V2 Int -> ((IO () -> IO ()) -> IO a) -> IO a
withWindow name size action = CC.runInBoundThread $ do
  _ <- SDL.init SDL.SDL_INIT_EVERYTHING >>= checkWhen (< 0)

  SDL.SDL_GL_CONTEXT_MAJOR_VERSION .= 4
  SDL.SDL_GL_CONTEXT_MINOR_VERSION .= 1
  SDL.SDL_GL_CONTEXT_PROFILE_MASK .= SDL.SDL_GL_CONTEXT_PROFILE_CORE

  SDL.SDL_GL_RED_SIZE   .= 8
  SDL.SDL_GL_GREEN_SIZE .= 8
  SDL.SDL_GL_BLUE_SIZE  .= 8
  SDL.SDL_GL_ALPHA_SIZE .= 8
  SDL.SDL_GL_DEPTH_SIZE .= 16

  SDL.SDL_GL_DOUBLEBUFFER .= fromEnum True

  ignoreEventsOfTypes
    [ SDL.SDL_FINGERMOTION
    , SDL.SDL_FINGERUP
    , SDL.SDL_FINGERDOWN ]

  C.withCString name $ \ name ->
    withSDLWindow name size flags $ \ window ->
      withSDLContext window $ \ _ ->
        action (\ draw -> forever $ do
          draw
          event <- waitEvent
          case eventPayload event of
            QuitEvent -> do
              quit
              exitSuccess
            _ -> pure ()
          SDL.glSwapWindow window) `E.finally` SDL.quit
  where flags = foldr (.|.) 0
          [ SDL.SDL_WINDOW_OPENGL
          , SDL.SDL_WINDOW_SHOWN
          , SDL.SDL_WINDOW_RESIZABLE
          , SDL.SDL_WINDOW_ALLOW_HIGHDPI ]


withSDLWindow :: C.CString -> Linear.V2 Int -> Word32 -> (SDL.Window -> IO a) -> IO a
withSDLWindow name (V2 w h) flags = E.bracket
  (SDL.createWindow name SDL.SDL_WINDOWPOS_CENTERED SDL.SDL_WINDOWPOS_CENTERED (fromIntegral w) (fromIntegral h) flags >>= checkNonNull)
  SDL.destroyWindow

withSDLContext :: SDL.Window -> (SDL.GLContext -> IO a) -> IO a
withSDLContext window = E.bracket
  (SDL.glCreateContext window >>= checkNonNull)
  SDL.glDeleteContext

checkSDLError :: IO ()
checkSDLError = do
  msg <- SDL.getError >>= C.peekCString
  SDL.clearError
  when (msg /= "") $ E.throw $ SDLException msg

checkWhen :: (a -> Bool) -> a -> IO a
checkWhen predicate value = do
  when (predicate value) checkSDLError
  pure value

checkNonNull :: Ptr a -> IO (Ptr a)
checkNonNull = checkWhen (== nullPtr)


newtype SDLException = SDLException String
  deriving (E.Exception, Show)


(.=) :: SDL.GLattr -> Int -> IO ()
attribute .= value = do
  result <- SDL.glSetAttribute attribute (fromIntegral value)
  _ <- checkWhen (< 0) result
  pure ()


ignoreEventsOfTypes :: [Word32] -> IO ()
ignoreEventsOfTypes = traverse_ (\ t -> SDL.eventState t 0 >>= checkWhen (/= 0))
