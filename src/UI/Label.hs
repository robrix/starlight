{-# LANGUAGE DataKinds, DisambiguateRecordFields, FlexibleContexts, NamedFieldPuns, TypeApplications, TypeOperators #-}
module UI.Label
( Label(colour, bcolour)
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
import Geometry.Rect
import GL.Array
import GL.Buffer
import GL.Effect.Program
import GL.Framebuffer as GL
import GL.Object
import qualified GL.Program as GL
import GL.Range (Range(Range))
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
import UI.Glyph (Instance(..), Run(..), geometry)

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
  , bounds  :: !(Rect Int)
  , scale   :: !Int
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

  scale <- Window.scale

  pure $ Label { textP, glyphP, colour = black, bcolour = Nothing, texture, fbuffer, glyphB, glyphA, quadA, bounds = Rect 0 0, scale }

setLabel
  :: ( HasCallStack
     , Has (Lift IO) sig m
     , Has Program sig m
     )
  => Label
  -> Font
  -> String
  -> m Label
setLabel l@Label { texture, fbuffer, glyphP, glyphB, glyphA, scale } font string = runLiftIO $ do
  glBlendFunc GL_ONE GL_ONE -- add

  let Run instances b = layoutString font string
      vertices = geometry . UI.Glyph.glyph =<< instances
      bounds = clamp b
      Rect (V2 x y) (V2 w h) = fromIntegral <$> scale *^ clamp b
      boundsSize = rectMax bounds - rectMin bounds

  bind (Just texture)
  setParameter Texture2D MagFilter Nearest
  setParameter Texture2D MinFilter Nearest
  setParameter Texture2D WrapS ClampToEdge
  setParameter Texture2D WrapT ClampToEdge
  setImageFormat Texture2D RGBA8 (scale *^ boundsSize) RGBA (Packed8888 True)

  bind (Just fbuffer)
  attachTexture (GL.Colour 0) texture

  glViewport x y w h
  glScissor  x y w h

  setClearColour transparent
  glClear GL_COLOR_BUFFER_BIT

  bind (Just glyphB)
  realloc glyphB (length vertices) Static Draw
  copy glyphB 0 vertices

  bind (Just glyphA)
  configureArray glyphB glyphA

  use glyphP $ do
    let V2 sx sy = fromIntegral scale / fmap fromIntegral boundsSize
    for_ instances $ \ Instance{ offset, range } ->
      for_ jitterPattern $ \ (colour, V2 tx ty) -> do
        set @"colour" colour
        set @"matrix3"
          $   translated (-1)
          !*! scaled     (V3 sx sy 1)
          !*! translated (V2 offset 0)
          !*! translated (V2 tx ty * (1 / fromIntegral scale))
        drawArrays Triangles range

  pure l { bounds = bounds } where
  jitterPattern
    = [ (red,   V2 (-1 / 12.0) (-5 / 12.0))
      , (red,   V2 ( 1 / 12.0) ( 1 / 12.0))
      , (green, V2 ( 3 / 12.0) (-1 / 12.0))
      , (green, V2 ( 5 / 12.0) ( 5 / 12.0))
      , (blue,  V2 ( 7 / 12.0) (-3 / 12.0))
      , (blue,  V2 ( 9 / 12.0) ( 3 / 12.0))
      ]


drawLabel
  :: ( HasCallStack
     , Has (Lift IO) sig m
     , Has Program sig m
     )
  => Label
  -> m ()
drawLabel Label { texture, textP, colour, bcolour, quadA, bounds, scale } = runLiftIO $ do
  glBlendFunc GL_ZERO GL_SRC_COLOR

  bind @Framebuffer Nothing

  let Rect (V2 x y) (V2 w h) = fromIntegral <$> scale *^ bounds
  glViewport x y w h
  glScissor  x y w h

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
      glBlendFunc GL_ONE GL_ONE
      set @"colour" colour
      drawArrays TriangleStrip range
