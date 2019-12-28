{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module UI.Label
( Label
, label
, prepareLabel
, setLabel
, drawLabel
) where

import           Control.Effect.Finally
import           Control.Effect.Lift
import           Control.Monad (when)
import           Control.Monad.IO.Class.Lift
import           Data.Coerce
import           Data.Foldable (foldl', for_)
import           Data.Functor.Identity
import           Data.Interval
import           Data.IORef
import qualified Data.Map as Map
import           Geometry.Rect
import           GHC.Stack
import           GL.Array
import           GL.Buffer
import           GL.Framebuffer as GL
import           GL.Object
import           GL.Program
import           GL.Texture
import           GL.TextureUnit
import           Graphics.GL.Core41
import           Lens.Micro ((^.))
import           Linear.Exts
import           Linear.Matrix
import           Linear.V2
import           Linear.V3
import           Linear.V4
import           Linear.Vector
import           UI.Colour
import qualified UI.Effect.Window as Window
import           UI.Font
import           UI.Glyph (Glyph(Glyph, char, geometry), Instance(..), Run(..))
import qualified UI.Label.Glyph as Glyph
import qualified UI.Label.Text as Text

newtype Label = Label { ref :: IORef LabelState }

data LabelState = LabelState
  { textP   :: !(Program Text.U  Text.V  Text.O)
  , glyphP  :: !(Program Glyph.U Glyph.V Glyph.O)
  , colour  :: !(Colour Float)
  , bcolour :: !(Maybe (Colour Float))
  , texture :: !(Texture 'Texture2D)
  , fbuffer :: !Framebuffer
  , glyphB  :: !(Buffer 'GL.Buffer.Array (Glyph.V Identity))
  , glyphA  :: !(Array (Glyph.V Identity))
  , quadA   :: !(Array (Text.V  Identity))
  , bounds  :: !(Rect Int)
  , scale   :: !Int
  , font    :: !Font
  , string  :: !String
  , chars   :: !(Map.Map Char (Interval Int))
  }


label
  :: ( Has Finally sig m
     , Has (Lift IO) sig m
     , Has Window.Window sig m
     , HasCallStack
     )
  => Font
  -> Colour Float
  -> m Label
label font colour = do
  texture <- gen1 @(Texture 'Texture2D)
  fbuffer <- gen1

  glyphP <- build Glyph.shader
  textP  <- build Text.shader

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
    copy buffer 0 (coerce vertices)

    bind (Just array)
    array <$ configureArray buffer array

  scale <- Window.scale

  Label <$> sendIO (newIORef LabelState { textP, glyphP, colour, bcolour = Nothing, texture, fbuffer, glyphB, glyphA, quadA, bounds = Rect 0 0, scale, font, string = "", chars = Map.empty })

-- | Cache the glyphs for the characters available to draw later on.
prepareLabel
  :: ( HasCallStack
     , Has (Lift IO) sig m
     )
  => Label
  -> String
  -> m ()
prepareLabel Label{ ref } string = runLiftIO $ do
  l@LabelState { glyphB, glyphA, bounds, font, string = oldString } <- sendIO (readIORef ref)

  when (bounds == Rect 0 0 || oldString /= string) $ do
    let (vs, chars, _) = foldl' combine (id, Map.empty, 0) (glyphsForString font string)
        combine (vs, cs, i) Glyph{ char, geometry } = let i' = i + length geometry in (vs . (geometry ++), Map.insert char (Interval i i') cs, i')
        vertices = vs []

    bind (Just glyphB)
    realloc glyphB (length vertices) Static Draw
    copy glyphB 0 (coerce vertices)

    bindArray glyphA $ configureArray glyphB glyphA

    sendIO (writeIORef ref l { chars })

-- | Set the label’s text.
--
-- Characters not in the string passed to 'prepareLabel' will not be drawn.
setLabel :: Has (Lift IO) sig m => Label -> String -> m ()
setLabel Label{ ref } string = runLiftIO $ do
  l@LabelState{ texture, fbuffer, glyphA, glyphP, scale, font, chars } <- sendIO (readIORef ref)

  glBlendFunc GL_ONE GL_ONE -- add

  let Run instances b = layoutString font chars string
      bounds = let b' = outsetToIntegralCoords (fontScale font *^ b) in Rect 0 (rectMax b' - rectMin b')

  bind (Just texture)
  setParameter Texture2D MagFilter Nearest
  setParameter Texture2D MinFilter Nearest
  setParameter Texture2D WrapS ClampToEdge
  setParameter Texture2D WrapT ClampToEdge
  setImageFormat Texture2D RGBA8 (scale *^ rectMax bounds) RGBA (Packed8888 True)

  bind (Just fbuffer)
  attachTexture (GL.Colour 0) texture

  viewport $ scale *^ bounds
  scissor  $ scale *^ bounds

  setClearColour transparent
  glClear GL_COLOR_BUFFER_BIT

  let V2 sx sy = fromIntegral scale / fmap fromIntegral (rectMax bounds)
  bindArray glyphA . use glyphP $ do
    set Glyph.U
      { matrix3   = Nothing
      , scale     = Just (1 / fromIntegral scale)
      , fontScale = Just (fontScale font)
      , offset    = Nothing
      }
    for_ instances $ \ Instance{ offset, range } -> do
      set Glyph.U
        { matrix3   = Just
            $   translated (-1)
            !*! scaled     (V3 sx sy 1)
        , scale     = Nothing
        , fontScale = Nothing
        , offset    = Just (V2 offset 0 + negated (rectMin b))
        }
      drawArraysInstanced Triangles range 6

  sendIO (writeIORef ref l { bounds, string })


drawLabel
  :: ( HasCallStack
     , Has Finally sig m
     , Has (Lift IO) sig m
     )
  => Label
  -> m ()
drawLabel Label{ ref } = runLiftIO $ do
  LabelState { texture, textP, colour, bcolour, quadA, bounds, scale } <- sendIO (readIORef ref)
  glBlendFunc GL_ZERO GL_SRC_COLOR

  bind @Framebuffer Nothing

  viewport $ scale *^ bounds
  scissor  $ scale *^ bounds

  case bcolour of
    Just colour -> do
      setClearColour colour
      glClear GL_COLOR_BUFFER_BIT
    _ -> pure ()

  use textP $ do
    let b = fromIntegral <$> bounds
        V2 w h = rectMax b - rectMin b
        rect = V4
          (b ^. _min . _x / w)
          (b ^. _max . _y / h)
          (b ^. _max . _x / w)
          (b ^. _min . _y / h)

    let textureUnit = TextureUnit 0
    setActiveTexture textureUnit
    bind (Just texture)

    set Text.U
      { rect    = Just rect
      , sampler = Just textureUnit
      , colour  = Just transparent
      }

    bindArray quadA $ do
      let range = Interval 0 4
      drawArrays TriangleStrip range

      when (opaque colour /= black) $ do
        glBlendFunc GL_ONE GL_ONE
        set Text.U
          { rect    = Just rect
          , sampler = Just textureUnit
          , colour  = Just colour
          }
        drawArrays TriangleStrip range
