{-# LANGUAGE DataKinds, NamedFieldPuns, OverloadedStrings, TypeApplications #-}
module Main
( main
) where

import Control.Carrier.Finally
import Control.Carrier.State.Strict
import Control.Monad
import Control.Monad.IO.Class
import Data.Foldable
import Data.List.NonEmpty (nonEmpty)
import Data.Semigroup.Foldable
import Data.Time.Clock
import Foreign.Ptr
import Geometry.Rect
import GHC.Stack
import GL.Array
import GL.Buffer
import GL.Carrier.Program.Live
import GL.Error
import GL.Object
import GL.Shader
import GL.Texture
import GL.TextureUnit
import GL.Uniform
import Graphics.GL.Core41
import Linear.Exts
import Linear.Matrix as Linear
import Linear.V2 as Linear
import Linear.V3 as Linear
import Linear.V4 as Linear
import Linear.Vector as Linear
import System.Directory
import UI.Colour
import UI.Font as Font
import UI.Glyph
import UI.Layer hiding (draw)
import UI.Carrier.Window

-- import qualified Codec.Picture as C
-- import qualified Codec.Picture.Types as C
-- import qualified Data.ByteString.Lazy as B
-- import qualified Foreign.Marshal.Alloc as A
-- import Foreign.Storable
-- import System.CPUTime

main :: HasCallStack => IO ()
main = evalState (Nothing :: Maybe UTCTime) $ do
  Just tahoma <- readTypeface "/System/Library/Fonts/Supplemental/Tahoma.ttf"
  let glyphs = Font.glyphs tahoma "hello"
      rect    = Var "rect"    :: Var (V4 Float)
      colour  = Var "colour"  :: Var (V4 Float)
      sampler = Var "sampler" :: Var TextureUnit
      matrix3 = Var "matrix3" :: Var (M33 Float)
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

  runWindow "Text" (fromIntegral <$> windowSize)
    . runFinally
    . runProgram @"glyph" [(Vertex, "glyph-vertex.glsl"), (Fragment, "glyph-fragment.glsl")]
    . runProgram @"text" [(Vertex, "text-vertex.glsl"), (Fragment, "text-fragment.glsl")]
    $ do
      texture <- gen1 @(Texture 'Texture2D)
      framebuffer <- gen1

      glyphBuffer <- gen1
      glyphArray <- gen1
      bind glyphBuffer $ do
        realloc glyphBuffer (length glyphVertices) Static GL.Buffer.Draw
        copy glyphBuffer 0 glyphVertices

        bind glyphArray $ configureArray glyphBuffer glyphArray

      screenQuadBuffer <- gen1
      screenQuadArray <- gen1
      bind screenQuadBuffer $ do
        realloc screenQuadBuffer (length screenQuadVertices) Static GL.Buffer.Draw
        copy screenQuadBuffer 0 screenQuadVertices

        bind screenQuadArray $ configureArray screenQuadBuffer screenQuadArray

      shipBuffer <- gen1
      shipArray <- gen1
      bind shipBuffer $ do
        realloc shipBuffer (length shipVertices) Static GL.Buffer.Draw
        copy shipBuffer 0 shipVertices

        bind shipArray $ configureArray shipBuffer shipArray

      bind texture $ do
        setMagFilter Texture2D Nearest
        setMinFilter Texture2D Nearest
        checkingGLError $ glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_S GL_CLAMP_TO_EDGE
        checkingGLError $ glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_T GL_CLAMP_TO_EDGE
        checkingGLError $ glTexImage2D GL_TEXTURE_2D 0 GL_RGBA8 (2 * width) (2 * height) 0 GL_RGBA GL_UNSIGNED_INT_8_8_8_8_REV nullPtr

      bind framebuffer $ do
        checkingGLError $ glFramebufferTexture2D GL_FRAMEBUFFER GL_COLOR_ATTACHMENT0 GL_TEXTURE_2D (unTexture texture) 0
        status <- glCheckFramebufferStatus GL_FRAMEBUFFER
        unless (status == GL_FRAMEBUFFER_COMPLETE) (throwGLError status)

      glEnable GL_BLEND
      glEnable GL_SCISSOR_TEST

      let drawGlyphs = do
            glBlendFunc GL_ONE GL_ONE -- add

            use @"glyph"

            -- set @"glyph" colour white
            -- set @"glyph" matrix3 identity
            -- bind screenQuadArray $
            --   traverse_ (drawArrays TriangleStrip) (arrayRanges screenQuadVertices)

            bind glyphArray $ do

              let V2 sx sy = V2 2 2 / fmap fromIntegral windowSize
                  windowScale = 1 / 2
              for_ (zip instances glyphRanges) $ \ (Instance{ offset, scale }, range) ->
                for_ jitterPattern $ \ (glyphColour, V2 tx ty) -> do
                  set @"glyph" colour glyphColour
                  set @"glyph" matrix3
                    $   translated (-1)
                    !*! scaled     (V3 sx sy 1)
                    !*! translated offset
                    !*! translated (V2 tx ty * windowScale)
                    !*! scaled     scale
                  drawArrays Triangles range

              -- let w = 2 * fromIntegral width
              --     h = 2 * fromIntegral height
              -- A.allocaBytes (4 * w * h) $ \ pixels -> do
              --   bind texture $ do
              --     checkingGLError $ glGetTexImage GL_TEXTURE_2D 0 GL_RGBA GL_UNSIGNED_INT_8_8_8_8_REV pixels
              --     checkingGLError $ glBindFramebuffer GL_READ_FRAMEBUFFER (unFramebuffer framebuffer)
              --     checkingGLError $ glReadPixels 0 0 (2 * width) (2 * height) GL_RGBA GL_UNSIGNED_INT_8_8_8_8_REV pixels
              --     image <- C.withImage w h $ \ x y -> do
              --       let pixel = pixels `plusPtr` (w * y + x)
              --       C.unpackPixel <$> peek pixel :: IO C.PixelRGBA8
              --     time <- getCPUTime
              --     B.writeFile ("test-" ++ show time ++ ".png") (C.encodePng image)

          drawText = do
            glBlendFunc GL_ZERO GL_SRC_COLOR

            -- print instanceBounds'

            use @"text"
            let rect' = V4
                  (fromIntegral (floor   (minX instanceBounds') :: Int) / fromIntegral width)
                  (fromIntegral (ceiling (maxY instanceBounds') :: Int) / fromIntegral height)
                  (fromIntegral (ceiling (maxX instanceBounds') :: Int) / fromIntegral width)
                  (fromIntegral (floor   (minY instanceBounds') :: Int) / fromIntegral height)

            -- print rect'

            set @"text" rect rect'
            -- set @"text" rect (V4 0 0 1 1)
            set @"text" colour transparent
            -- set @"text" colour black
            let textureUnit = TextureUnit 0
            setActiveTexture textureUnit
            bind texture $ do
              set @"text" sampler textureUnit

              bind screenQuadArray $ do
                traverse_ (drawArrays TriangleStrip) screenQuadRanges

                when (opaque textColour /= black) $ do
                  glBlendFunc GL_ONE GL_ONE
                  set @"text" colour textColour
                  traverse_ (drawArrays TriangleStrip) screenQuadRanges

          drawShip = do
            bind shipArray $
              traverse_ (drawArrays LineLoop) shipRanges

      draw $ do
        prev <- get
        current <- liftIO (Just <$> getModificationTime "text-vertex.glsl")
        when (prev /= current) $ do
          liftIO $ putStrLn "text-vertex.glsl modified"
          put current
        traverse_ drawLayer
          [ Layer (Just framebuffer) transparent (Rect 0 windowSize) drawGlyphs
          , Layer Nothing black (Rect 0 windowSize) drawText
          , Layer Nothing blue (Rect ((`div` 4) <$> windowSize) ((`div` 2) <$> windowSize)) drawShip
          ]

  where jitterPattern
          = [ (red,   V2 (-1 / 12.0) (-5 / 12.0))
            , (red,   V2 ( 1 / 12.0) ( 1 / 12.0))
            , (green, V2 ( 3 / 12.0) (-1 / 12.0))
            , (green, V2 ( 5 / 12.0) ( 5 / 12.0))
            , (blue,  V2 ( 7 / 12.0) (-3 / 12.0))
            , (blue,  V2 ( 9 / 12.0) ( 3 / 12.0))
            ]
        windowSize = V2 width height
        width  = 1024
        height = 768

        textColour = white

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
