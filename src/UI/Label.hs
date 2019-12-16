{-# LANGUAGE DataKinds, DisambiguateRecordFields, FlexibleContexts, NamedFieldPuns, TypeApplications, TypeOperators #-}
module UI.Label
( Label
, label
, setLabel
, drawLabel
) where

import Control.Effect.Finally
import Control.Effect.Lift
import Control.Monad (when)
import Control.Monad.IO.Class.Lift
import Data.Foldable (for_)
import GHC.Stack
import GL.Array
import GL.Buffer
import GL.Effect.Program
import GL.Framebuffer as GL
import GL.Object
import qualified GL.Program as GL
import GL.Shader
import GL.Texture
import GL.TextureUnit
import Graphics.GL.Core41
import Linear.Exts
import Linear.Matrix
import Linear.V2
import Linear.V3
import Linear.V4
import Linear.Vector
import UI.Colour
import qualified UI.Effect.Window as Window
import UI.Font
import UI.Glyph

data Label = Label
  { textP   :: !(GL.Program
    '[ "rect"    '::: V4 Float
     , "sampler" '::: TextureUnit
     , "colour"  '::: V4 Float ])
  , glyphP  :: !(GL.Program
    '[ "matrix3" '::: M33 Float
     , "colour"  '::: V4 Float ])
  , colour  :: !(Colour Float)
  , bcolour :: !(Maybe (Colour Float))
  , texture :: !(Texture 'Texture2D)
  , fbuffer :: !Framebuffer
  , glyphB  :: !(Buffer 'GL.Buffer.Array (V4 Float))
  , glyphA  :: !(Array (V4 Float))
  , quadA   :: !(Array (V2 Float))
  }


label
  :: ( HasCallStack
     , Has Finally sig m
     , Has (Lift IO) sig m
     , Has Program sig m
     , Has Window.Window sig m
     )
  => m Label
label = do
  texture <- gen1 @(Texture 'Texture2D)
  fbuffer <- gen1

  glyphP <- build
    @'[ "matrix3" '::: M33 Float
      , "colour"  '::: V4 Float ]
    [(Vertex, "glyph-vertex.glsl"), (Fragment, "glyph-fragment.glsl")]
  textP  <- build
    @'[ "rect"    '::: V4 Float
      , "sampler" '::: TextureUnit
      , "colour"  '::: V4 Float ]
    [(Vertex, "text-vertex.glsl"),  (Fragment, "text-fragment.glsl")]

  glyphA <- gen1
  glyphB <- gen1

  bind (Just glyphA)
  configureArray glyphB glyphA

  quadA <- do
    let vertices =
          [ V2 (-1) (-1)
          , V2   1  (-1)
          , V2 (-1)   1
          , V2   1    1  :: V2 Float
          ]

    buffer <- gen1
    array  <- gen1

    bind (Just buffer)
    realloc buffer (length vertices) Static Draw
    copy buffer 0 vertices

    bind (Just array)
    array <$ configureArray buffer array

  bind (Just texture)
  setParameter Texture2D MagFilter Nearest
  setParameter Texture2D MinFilter Nearest
  scale <- Window.scale
  size  <- Window.size
  setParameter Texture2D WrapS ClampToEdge
  setParameter Texture2D WrapT ClampToEdge
  setImageFormat Texture2D RGBA8 (scale *^ size) RGBA (Packed8888 True)

  bind (Just fbuffer)
  attachTexture (GL.Colour 0) texture

  pure $ Label { textP, glyphP, colour = black, bcolour = Nothing, texture, fbuffer, glyphB, glyphA, quadA }

setLabel
  :: ( Has (Lift IO) sig m
     , Has Program sig m
     , Has Window.Window sig m
     )
  => Label
  -> Font
  -> String
  -> m ()
setLabel Label { fbuffer, glyphP, glyphB, glyphA } font string = do
  runLiftIO $ glBlendFunc GL_ONE GL_ONE -- add

  bind (Just fbuffer)

  let Run instances _ = layoutString font string
      (vertices, ranges) = combineGeometry (geometry . UI.Glyph.glyph <$> instances)

  bind (Just glyphB)
  realloc glyphB (length vertices) Static Draw
  copy glyphB 0 vertices

  use glyphP $ do
    windowScale <- Window.scale
    windowSize  <- Window.size

    bind (Just glyphA)
    let V2 sx sy = windowScale / windowSize
    for_ (zip instances ranges) $ \ (Instance{ offset }, range) ->
      for_ jitterPattern $ \ (colour, V2 tx ty) -> do
        set @"colour" colour
        set @"matrix3"
          $   translated (-1)
          !*! scaled     (V3 sx sy 1)
          !*! translated offset
          !*! translated (V2 tx ty * (1 / windowScale))
        drawArrays Triangles range where
  jitterPattern
    = [ (red,   V2 (-1 / 12.0) (-5 / 12.0))
      , (red,   V2 ( 1 / 12.0) ( 1 / 12.0))
      , (green, V2 ( 3 / 12.0) (-1 / 12.0))
      , (green, V2 ( 5 / 12.0) ( 5 / 12.0))
      , (blue,  V2 ( 7 / 12.0) (-3 / 12.0))
      , (blue,  V2 ( 9 / 12.0) ( 3 / 12.0))
      ]


drawLabel
  :: ( Has (Lift IO) sig m
     , Has Program sig m
     )
  => Label
  -> m ()
drawLabel Label { texture, textP, colour, bcolour, quadA } = do
  runLiftIO (glBlendFunc GL_ZERO GL_SRC_COLOR)

  bind @Framebuffer Nothing

  case bcolour of
    Just colour -> do
      setClearColour colour
      glClear GL_COLOR_BUFFER_BIT
    _ -> pure ()

  use textP $ do
    set @"rect" $ V4 0 1 1 0

    set @"colour" transparent

    let textureUnit = TextureUnit 0
    setActiveTexture textureUnit
    bind (Just texture)

    set @"sampler" textureUnit

    bind (Just quadA)
    let range = Range 0 4
    drawArrays TriangleStrip range

    when (opaque colour /= black) $ do
      runLiftIO $ glBlendFunc GL_ONE GL_ONE
      set @"colour" colour
      drawArrays TriangleStrip range


combineGeometry :: [[v n]] -> ([v n], [Range])
combineGeometry = go 0 where
  go _ [] = ([], [])
  go prevIndex (geometry : rest) =
    let count = length geometry
        (vertices, ranges) = go (prevIndex + count) rest
    in (geometry <> vertices, Range prevIndex count : ranges)
