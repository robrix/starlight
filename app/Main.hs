{-# LANGUAGE DataKinds, FlexibleContexts, NamedFieldPuns, OverloadedStrings, TypeApplications, TypeOperators #-}
module Main
( main
) where

import Control.Carrier.Finally
import Control.Carrier.State.Strict
import Control.Carrier.Time
import Control.Effect.Lift
import Control.Monad
import Data.Foldable
import Data.List.NonEmpty (nonEmpty)
import Data.Semigroup.Foldable
import Foreign.Ptr
import Foreign.Storable (Storable)
import Geometry.Rect
import GHC.Stack
import GHC.TypeLits
import GL.Array
import GL.Buffer
import GL.Carrier.Program.Live
import GL.Error
import GL.Object
import GL.Scalar
import GL.Shader
import GL.Texture
import GL.TextureUnit
import Graphics.GL.Core41
import qualified Lens.Micro as Lens
import Linear.Exts
import Linear.Matrix as Linear
import Linear.V (Size)
import Linear.V2 as Linear
import Linear.V3 as Linear
import Linear.V4 as Linear
import Linear.Vector as Linear
import qualified SDL
import UI.Colour
import UI.Font as Font
import UI.Glyph
import UI.Layer hiding (draw)
import qualified UI.Carrier.Window as Window

-- import qualified Codec.Picture as C
-- import qualified Codec.Picture.Types as C
-- import qualified Data.ByteString.Lazy as B
-- import qualified Foreign.Marshal.Alloc as A
-- import Foreign.Storable
-- import System.CPUTime

main :: HasCallStack => IO ()
main = do
  Just tahoma <- readTypeface "/System/Library/Fonts/Supplemental/Tahoma.ttf"
  let glyphs = Font.glyphs tahoma "hello"
      instances = combineInstances (V2 288 288) (V2 0 0) glyphs
      instanceBounds' = maybe (Rect zero zero) (getUnion . foldMap1 (Union . instanceBounds)) (nonEmpty instances)
      (shipVertices, shipRanges) = combineGeometry
        [ [ V3 0      1      0
          , V3 (-0.5) 0      0
          , V3 0      (-0.5) 0
          , V3 0.5    0      0 :: V3 Float
          ]
        , [ V3 0 1 0
          , V3 (-1) 0 0
          , V3 0 (-1) 0
          , V3 1 0 0
          ]
        ]
      (screenQuadVertices, screenQuadRanges) = combineGeometry
        [ [ V2 (-1) (-1)
          , V2   1  (-1)
          , V2 (-1)   1
          , V2   1    1  :: V2 Float
          ]
        ]
      (glyphVertices, glyphRanges) = combineGeometry (geometry . glyph <$> instances)

  Window.runWindow "Starlight" (V2 1024 768)
    . runFinally
    . runTime
    . runProgram
    . evalState PlayerState { position = 0, velocity = 0, acceleration = 0, rotation = 0 }
    $ do
      glyph <- build @'[ "matrix3" '::: M33 Float, "colour" '::: V4 Float ]
        [(Vertex, "glyph-vertex.glsl"), (Fragment, "glyph-fragment.glsl")]
      text  <- build @'[ "rect" '::: V4 Float, "sampler" '::: TextureUnit, "colour" '::: V4 Float ]
        [(Vertex, "text-vertex.glsl"),  (Fragment, "text-fragment.glsl")]
      stars <- build @'[ "resolution" '::: V3 Float, "time" '::: Float, "origin" '::: V2 Float ]
        [(Vertex, "stars-vertex.glsl"), (Fragment, "stars-fragment.glsl")]
      ship <- build
        @'[ "colour"      '::: V4 Float
          , "translation" '::: V2 Float
          , "scale"       '::: V2 Float
          , "rotation"    '::: Float ]
        [(Vertex, "ship-vertex.glsl"), (Fragment, "ship-fragment.glsl")]

      startTime <- now

      texture <- gen1 @(Texture 'Texture2D)
      framebuffer <- gen1

      (_, glyphArray) <- loadVertices glyphVertices
      (_, screenQuadArray) <- loadVertices screenQuadVertices
      (_, shipArray) <- loadVertices shipVertices

      bind (Just texture)
      setMagFilter Texture2D Nearest
      setMinFilter Texture2D Nearest
      scale <- Window.scale
      V2 width height <- Window.size
      checkingGLError $ glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_S GL_CLAMP_TO_EDGE
      checkingGLError $ glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_T GL_CLAMP_TO_EDGE
      checkingGLError $ glTexImage2D GL_TEXTURE_2D 0 GL_RGBA8 (scale * width) (scale * height) 0 GL_RGBA GL_UNSIGNED_INT_8_8_8_8_REV nullPtr

      bind (Just framebuffer)
      checkingGLError $ glFramebufferTexture2D GL_FRAMEBUFFER GL_COLOR_ATTACHMENT0 GL_TEXTURE_2D (unTexture texture) 0
      status <- glCheckFramebufferStatus GL_FRAMEBUFFER
      unless (status == GL_FRAMEBUFFER_COMPLETE) (throwGLError status)

      glEnable GL_BLEND
      glEnable GL_SCISSOR_TEST

      let drawGlyphs = do
            glBlendFunc GL_ONE GL_ONE -- add

            use glyph $ do

              -- set @"colour" white
              -- set @"matrix3" identity
              -- bind screenQuadArray $
              --   traverse_ (drawArrays TriangleStrip) (arrayRanges screenQuadVertices)
              windowScale <- Window.scale
              windowSize <- Window.size

              bind (Just glyphArray)
              let V2 sx sy = windowScale / windowSize
              for_ (zip instances glyphRanges) $ \ (Instance{ offset, scale }, range) ->
                for_ jitterPattern $ \ (glyphColour, V2 tx ty) -> do
                  set @"colour" glyphColour
                  set @"matrix3"
                    $   translated (-1)
                    !*! scaled     (V3 sx sy 1)
                    !*! translated offset
                    !*! translated (V2 tx ty * (1 / windowScale))
                    !*! scaled     scale
                  drawArrays Triangles range

              -- let w = scale * fromIntegral width
              --     h = scale * fromIntegral height
              -- A.allocaBytes (4 * w * h) $ \ pixels -> do
              --   bind texture $ do
              --     checkingGLError $ glGetTexImage GL_TEXTURE_2D 0 GL_RGBA GL_UNSIGNED_INT_8_8_8_8_REV pixels
              --     checkingGLError $ glBindFramebuffer GL_READ_FRAMEBUFFER (unFramebuffer framebuffer)
              --     checkingGLError $ glReadPixels 0 0 (scale * width) (scale * height) GL_RGBA GL_UNSIGNED_INT_8_8_8_8_REV pixels
              --     image <- C.withImage w h $ \ x y -> do
              --       let pixel = pixels `plusPtr` (w * y + x)
              --       C.unpackPixel <$> peek pixel :: IO C.PixelRGBA8
              --     time <- getCPUTime
              --     B.writeFile ("test-" ++ show time ++ ".png") (C.encodePng image)

          drawText = do
            glBlendFunc GL_ZERO GL_SRC_COLOR

            -- print instanceBounds'

            use text $ do
              let rect' = V4
                    (fromIntegral (floor   (minX instanceBounds') :: Int) / fromIntegral width)
                    (fromIntegral (ceiling (maxY instanceBounds') :: Int) / fromIntegral height)
                    (fromIntegral (ceiling (maxX instanceBounds') :: Int) / fromIntegral width)
                    (fromIntegral (floor   (minY instanceBounds') :: Int) / fromIntegral height)

              -- print rect'

              set @"rect" rect'
              -- set @"rect" (V4 0 0 1 1)
              set @"colour" transparent
              -- set @"colour" black
              let textureUnit = TextureUnit 0
              setActiveTexture textureUnit
              bind (Just texture)

              set @"sampler" textureUnit

              bind (Just screenQuadArray)

              traverse_ (drawArrays TriangleStrip) screenQuadRanges

              when (opaque textColour /= black) $ do
                glBlendFunc GL_ONE GL_ONE
                set @"colour" textColour
                traverse_ (drawArrays TriangleStrip) screenQuadRanges

          drawCanvas = do
            windowScale <- Window.scale
            windowSize <- Window.size
            events <- Window.poll

            when (any ((== SDL.QuitEvent) . SDL.eventPayload) events) Window.stop

            PlayerState{ rotation } <- get

            delta <- fromRational . toRational <$> since startTime

            let theta = delta * foldl' accumRotation 0 events + rotation
                scale = windowScale / windowSize
                V2 width height = windowSize

            modify (_rotation Lens..~ theta)

            use stars $ do
              set @"resolution" (V3 width height 8)
              set @"time" delta

              traverse_ (drawArrays TriangleStrip) screenQuadRanges

            bind (Just shipArray)

            use ship $ do
              set @"colour" $ V4 1 1 1 1
              set @"translation" 0
              set @"scale" (scale * 100)
              set @"rotation" theta

              traverse_ (drawArrays LineLoop) shipRanges

      Window.loop $ do
        windowSize <- Window.size
        let rect = Rect 0 windowSize
        traverse_ drawLayer
          [ Layer (Just framebuffer) (Just transparent) rect drawGlyphs
          , Layer Nothing (Just black) rect drawText
          , Layer Nothing (Just black) rect drawCanvas
          ]

  where jitterPattern
          = [ (red,   V2 (-1 / 12.0) (-5 / 12.0))
            , (red,   V2 ( 1 / 12.0) ( 1 / 12.0))
            , (green, V2 ( 3 / 12.0) (-1 / 12.0))
            , (green, V2 ( 5 / 12.0) ( 5 / 12.0))
            , (blue,  V2 ( 7 / 12.0) (-3 / 12.0))
            , (blue,  V2 ( 9 / 12.0) ( 3 / 12.0))
            ]

        textColour = white

data PlayerState = PlayerState
  { position     :: !(V2 Float)
  , velocity     :: !(V2 Float)
  , acceleration :: !(V2 Float)
  , rotation     :: !Float
  }
  deriving (Eq, Ord, Show)

_position :: Lens.Lens' PlayerState (V2 Float)
_position = Lens.lens position (\ s v -> s { position = v })

_velocity :: Lens.Lens' PlayerState (V2 Float)
_velocity = Lens.lens velocity (\ s v -> s { velocity = v })

_acceleration :: Lens.Lens' PlayerState (V2 Float)
_acceleration = Lens.lens acceleration (\ s v -> s { acceleration = v })

_rotation :: Lens.Lens' PlayerState Float
_rotation = Lens.lens rotation (\ s r -> s { rotation = r })


accumRotation :: Float -> SDL.Event -> Float
accumRotation theta event = case SDL.eventPayload event of
  SDL.KeyboardEvent (SDL.KeyboardEventData _ SDL.Pressed _ (SDL.Keysym _ SDL.KeycodeLeft _)) -> theta + 1
  SDL.KeyboardEvent (SDL.KeyboardEventData _ SDL.Pressed _ (SDL.Keysym _ SDL.KeycodeRight _)) -> theta - 1
  _ -> theta


combineInstances :: V2 Float -> V2 Float -> [Glyph] -> [Instance]
combineInstances (V2 sx sy) = go where
  go offset (g:gs)
    = Instance g offset scale
    : go (offset + V2 (advanceWidth g * sx) 0) gs
  go _ [] = []
  scale = V3 sx sy 1

combineGeometry :: [[v n]] -> ([v n], [Range])
combineGeometry = go 0
  where go _ [] = ([], [])
        go prevIndex (geometry : rest) =
          let count = length geometry
              (vertices, ranges) = go (prevIndex + count) rest
          in (geometry <> vertices, Range prevIndex count : ranges)

loadVertices :: (KnownNat (Size v), Storable (v n), Scalar n, Has Finally sig m, Has (Lift IO) sig m) => [v n] -> m (Buffer 'GL.Buffer.Array (v n), Array (v n))
loadVertices vertices = do
  buffer <- gen1
  array <- gen1

  bind (Just buffer)
  realloc buffer (length vertices) Static GL.Buffer.Draw
  copy buffer 0 vertices

  bind (Just array)
  configureArray buffer array
  pure (buffer, array)
