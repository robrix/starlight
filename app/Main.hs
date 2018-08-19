{-# LANGUAGE GADTs, FlexibleInstances, RecordWildCards, ScopedTypeVariables #-}
module Main where

import qualified Control.Concurrent as CC
import qualified Control.Exception as E
import Control.Monad
import Data.Bits
import Data.Foldable
import Data.List.NonEmpty (nonEmpty)
import Data.Semigroup.Foldable
import Data.Word
import qualified Foreign.C.String as C
import Foreign.Ptr
import Geometry.Rect
import GHC.Stack
import GL.Array
import GL.Error
import GL.Framebuffer
import GL.Object
import GL.Program
import GL.Scalar
import GL.Shader
import GL.Texture
import GL.TextureUnit
import GL.Uniform
import Graphics.GL.Core41
import Graphics.GL.Types
import Linear.Matrix as Linear
import Linear.V2 as Linear
import Linear.V3 as Linear
import Linear.V4 as Linear
import Linear.Vector as Linear
import SDL.Event
import SDL.Init
import qualified SDL.Raw as SDL
import System.Exit
import UI.Colour
import UI.Font as Font
import UI.Glyph

main :: HasCallStack => IO ()
main = do
  Just tahoma <- readTypeface "/Library/Fonts/Tahoma.ttf"
  let glyphs = Font.glyphs tahoma "s"
  [textVertex, textFragment, glyphVertex, glyphFragment] <- traverse readFile ["text-vertex.glsl", "text-fragment.glsl", "glyph-vertex.glsl", "glyph-fragment.glsl"]
  CC.runInBoundThread $ C.withCString "Text" $ \ name -> do
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

    withWindow name (fromIntegral <$> windowSize) flags $ \ window ->
      withContext window $ \ _ ->
        let rect    = Var "rect"    :: Var (V4 Float)
            colour  = Var "colour"  :: Var (V4 Float)
            sampler = Var "sampler" :: Var TextureUnit
            matrix3 = Var "matrix3" :: Var (M33 Float)
            instances = combineInstances (V2 288 288) (V2 0 0) glyphs
            instanceBounds' = maybe (Rect zero zero) (getUnion . foldMap1 (Union . instanceBounds)) (nonEmpty instances)
            geometry = Geometry GL_TRIANGLES . instanceGeometry <$> instances
            vertices = foldl combineGeometry (ArrayVertices [] 0 []) (Geometry GL_TRIANGLE_STRIP
              [ V4 (-1) (-1) 0 1
              , V4   1  (-1) 0 1
              , V4 (-1)   1  0 1
              , V4   1    1  0 1 :: V4 Float
              ] : geometry) in
        withArray (arrayVertices vertices) $ \ array ->
        withBuiltProgram [(Vertex, textVertex), (Fragment, textFragment)] $ \ textProgram ->
        withBuiltProgram [(Vertex, glyphVertex), (Fragment, glyphFragment)] $ \ glyphProgram ->
        with $ \ texture ->
        with $ \ framebuffer ->
        forever $ do
          glViewport 0 0 (2 * width) (2 * height)

          glDisable GL_BLEND
          glBlendFunc GL_ONE GL_ZERO -- copy

          setClearColour white
          glClear GL_COLOR_BUFFER_BIT

          let V2 sx sy = V2 2 (-2) / fmap fromIntegral windowSize

          checkingGLError $ glBindFramebuffer GL_FRAMEBUFFER (unFramebuffer framebuffer)
          checkingGLError $ glBindTexture GL_TEXTURE_2D (unTexture texture)
          checkingGLError $ glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_NEAREST
          checkingGLError $ glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_NEAREST
          checkingGLError $ glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_S GL_CLAMP_TO_EDGE
          checkingGLError $ glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_T GL_CLAMP_TO_EDGE
          checkingGLError $ glTexImage2D GL_TEXTURE_2D 0 GL_RGBA width height 0 GL_RGBA GL_UNSIGNED_BYTE nullPtr

          checkingGLError $ glFramebufferTexture2D GL_FRAMEBUFFER GL_COLOR_ATTACHMENT0 GL_TEXTURE_2D (unTexture texture) 0

          glViewport 0 0 (2 * width) (2 * height)

          setClearColour transparent
          glClear GL_COLOR_BUFFER_BIT

          glEnable GL_BLEND
          glBlendFunc GL_ONE GL_ONE -- add

          checkingGLError $ glBindVertexArray (unArray array)

          useProgram glyphProgram
          for_ (zip instances (tail (arrayRanges vertices))) $ \ (Instance{..}, range) ->
            for_ (zip [0..] jitterPattern) $ \ (j, V2 tx ty) -> do
              when (j `mod` 2 == (0 :: Int)) $
                setUniformValue glyphProgram colour (V4 (if j == 0 then 1 else 0)
                                                        (if j == 2 then 1 else 0)
                                                        (if j == 4 then 1 else 0)
                                                        1)
              setUniformValue glyphProgram matrix3
                $   translated (V2 (-1) 1)
                !*! scaled     (V3 sx sy 1)
                !*! translated instanceOffset
                !*! translated (V2 tx ty / 2)
                !*! scaled     instanceScale
              drawRange range

          checkingGLError $ glBindFramebuffer GL_FRAMEBUFFER 0
          glBlendFunc GL_ZERO GL_SRC_COLOR

          useProgram textProgram
          let rect' = V4
                (    fromIntegral (floor   (minX instanceBounds') :: Int) / fromIntegral width)
                (1 - fromIntegral (ceiling (maxY instanceBounds') :: Int) / fromIntegral height)
                (    fromIntegral (ceiling (maxX instanceBounds') :: Int) / fromIntegral width)
                (1 - fromIntegral (floor   (minY instanceBounds') :: Int) / fromIntegral height)

          setUniformValue textProgram rect rect'
          setUniformValue textProgram colour transparent
          glActiveTexture GL_TEXTURE0
          glBindTexture GL_TEXTURE_2D (unTexture texture)
          setUniformValue textProgram sampler (TextureUnit 0)

          drawRange (head (arrayRanges vertices))
          -- traverse_ drawRange (tail (arrayRanges vertices))

          when (opaque textColour /= black) $ do
            glBlendFunc GL_ONE GL_ONE
            setUniformValue textProgram colour textColour
            drawRange (head (arrayRanges vertices))

          event <- waitEvent
          case eventPayload event of
            QuitEvent -> do
              quit
              exitSuccess
            _ -> pure ()

          SDL.glSwapWindow window
    `E.finally`
      SDL.quit
  where flags = foldr (.|.) 0
          [ SDL.SDL_WINDOW_OPENGL
          , SDL.SDL_WINDOW_SHOWN
          , SDL.SDL_WINDOW_RESIZABLE
          , SDL.SDL_WINDOW_ALLOW_HIGHDPI ]
        drawRange :: HasCallStack => ArrayRange -> IO ()
        drawRange (ArrayRange mode from count) = checkingGLError $ glDrawArrays mode (fromIntegral from) (fromIntegral count)
        jitterPattern
          = [ V2 (-1 / 12.0) (-5 / 12.0)
            , V2 ( 1 / 12.0) ( 1 / 12.0)
            , V2 ( 3 / 12.0) (-1 / 12.0)
            , V2 ( 5 / 12.0) ( 5 / 12.0)
            , V2 ( 7 / 12.0) (-3 / 12.0)
            , V2 ( 9 / 12.0) ( 3 / 12.0)
            ]
        windowSize = V2 width height
        width  = 1024
        height = 768

        textColour = V4 1 0 1 1

translated :: V2 Float -> M33 Float
translated (V2 tx ty) = V3 (V3 1 0 tx)
                           (V3 0 1 ty)
                           (V3 0 0 1)


combineInstances :: V2 Float -> V2 Float -> [Glyph] -> [Instance]
combineInstances scale@(V2 sx sy) offset (g:gs)
  = Instance g offset (V3 sx sy 1)
  : combineInstances scale (offset + V2 (glyphAdvanceWidth g) 0) gs
combineInstances _     _      []     = []

combineGeometry :: ArrayVertices (v n) -> Geometry (v n) -> ArrayVertices (v n)
combineGeometry ArrayVertices{..} (Geometry mode vertices) =
  let count = length vertices
  in ArrayVertices
    (arrayVertices <> vertices)
    (prevIndex + count)
    (arrayRanges <> [ ArrayRange { mode = mode, firstVertexIndex = prevIndex, vertexCount = count } ])


ignoreEventsOfTypes :: [Word32] -> IO ()
ignoreEventsOfTypes = traverse_ (\ t -> SDL.eventState t 0 >>= checkWhen (/= 0))

withWindow :: C.CString -> Linear.V2 Int -> Word32 -> (SDL.Window -> IO a) -> IO a
withWindow name (V2 w h) flags = E.bracket
  (SDL.createWindow name SDL.SDL_WINDOWPOS_CENTERED SDL.SDL_WINDOWPOS_CENTERED (fromIntegral w) (fromIntegral h) flags >>= checkNonNull)
  SDL.destroyWindow

withContext :: SDL.Window -> (SDL.GLContext -> IO a) -> IO a
withContext window = E.bracket
  (SDL.glCreateContext window >>= checkNonNull)
  SDL.glDeleteContext

checkWhen :: (a -> Bool) -> a -> IO a
checkWhen predicate value = do
  when (predicate value) checkSDLError
  pure value

checkNonNull :: Ptr a -> IO (Ptr a)
checkNonNull = checkWhen (== nullPtr)

checkSDLError :: IO ()
checkSDLError = do
  msg <- SDL.getError >>= C.peekCString
  SDL.clearError
  when (msg /= "") $ E.throw $ SDLException msg

(.=) :: SDL.GLattr -> Int -> IO ()
attribute .= value = do
  result <- SDL.glSetAttribute attribute (fromIntegral value)
  _ <- checkWhen (< 0) result
  pure ()

newtype SDLException = SDLException String
  deriving (Show)

instance E.Exception SDLException

data ArrayRange = ArrayRange { mode :: GLuint, firstVertexIndex :: Int, vertexCount :: Int }

data GeometryArray n = GeometryArray { geometryRanges :: [ArrayRange], geometryArray :: Array n }

data ArrayVertices a = ArrayVertices { arrayVertices :: [a], prevIndex :: Int, arrayRanges :: [ArrayRange] }

data Geometry a where
  Geometry :: (Foldable v, Scalar n) => GLuint -> [v n] -> Geometry (v n)
