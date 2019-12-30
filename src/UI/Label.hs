{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module UI.Label
( Label
, label
, labelSize
, setLabel
, drawLabel
) where

import           Control.Effect.Finally
import           Control.Effect.Lift
import           Control.Monad (when)
import           Control.Monad.IO.Class.Lift
import           Data.Coerce
import           Data.Foldable (for_)
import           Data.Functor.I
import           Data.Functor.Interval as Interval
import           Data.IORef
import           GHC.Stack
import           GL.Array
import           GL.Buffer
import           GL.Framebuffer as GL
import           GL.Object
import           GL.Program
import           GL.Shader.DSL (defaultVars)
import           GL.Texture
import           GL.TextureUnit
import           GL.Viewport
import           Graphics.GL.Core41
import           Linear.Exts
import           Linear.Matrix
import           Linear.V2
import           Linear.V3
import           Linear.Vector
import           UI.Colour
import qualified UI.Effect.Window as Window
import           UI.Glyph (Instance(..), Run(..))
import qualified UI.Label.Glyph as Glyph
import qualified UI.Label.Text as Text
import           UI.Typeface

data Label = Label
  { textP   :: !(Program Text.U  Text.V  Text.O)
  , texture :: !(Texture 'Texture2D)
  , fbuffer :: !Framebuffer
  , quadA   :: !(Array (Text.V  I))
  , scale   :: !Int
  , ref     :: !(IORef (Maybe LabelState))
  }

data LabelState = LabelState
  { size   :: !(V2 Int)
  , string :: !String
  }


label
  :: ( Has Finally sig m
     , Has (Lift IO) sig m
     , Has Window.Window sig m
     , HasCallStack
     )
  => m Label
label = do
  texture <- gen1 @(Texture 'Texture2D)
  fbuffer <- gen1

  textP  <- build Text.shader

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

  ref <- sendIO (newIORef Nothing)
  pure Label{ textP, texture, fbuffer, quadA, ref, scale }

labelSize :: Has (Lift IO) sig m => Label -> m (V2 Int)
labelSize = sendM . fmap (maybe (V2 0 0) UI.Label.size) . readIORef . ref


-- | Set the label’s text.
setLabel :: (HasCallStack, Has (Lift IO) sig m) => Label -> Font -> String -> m ()
setLabel Label{ texture, fbuffer, scale, ref } font string
  | null string = sendM (writeIORef ref Nothing)
  | otherwise   = runLiftIO $ do
    state <- sendIO (readIORef ref)
    case state of
      Just LabelState{ string = oldString } | string == oldString -> pure ()
      _ -> do
        glBlendFunc GL_ONE GL_ONE -- add

        Run instances b <- layoutString (face font) string

        let b' = Interval (pure floor) (pure ceiling) <*> fontScale font *^ b
            size = Interval.size b'

        bind (Just texture)
        setParameter Texture2D MagFilter Nearest
        setParameter Texture2D MinFilter Nearest
        setParameter Texture2D WrapS ClampToEdge
        setParameter Texture2D WrapT ClampToEdge
        setImageFormat Texture2D RGBA8 (scale *^ size) RGBA (Packed8888 True)

        bind (Just fbuffer)
        attachTexture (GL.Colour 0) texture

        viewport $ scale *^ Interval 0 size
        scissor  $ scale *^ Interval 0 size

        setClearColour transparent
        glClear GL_COLOR_BUFFER_BIT

        let V2 sx sy = fromIntegral scale / fmap fromIntegral size
        drawingGlyphs (face font) $ do
          set defaultVars
            { Glyph.scale     = Just (1 / fromIntegral scale)
            , Glyph.fontScale = Just (fontScale font)
            , Glyph.matrix    = Just
              $   translated (-1)
              !*! scaled     (V3 sx sy 1)
              !*! translated (fromIntegral <$> negated (min_ b'))
            }
          for_ instances $ \ Instance{ offset, range } -> do
            set defaultVars
              { Glyph.offset = Just offset
              }
            drawArraysInstanced Triangles range 6

        sendIO (writeIORef ref (Just LabelState{ UI.Label.size, string }))


drawLabel
  :: ( HasCallStack
     , Has Finally sig m
     , Has (Lift IO) sig m
     )
  => Label
  -> V2 Int
  -> Colour Float
  -> Maybe (Colour Float)
  -> m ()
drawLabel Label{ texture, textP, quadA, scale, ref } offset colour bcolour = runLiftIO $ do
  state <- sendIO (readIORef ref)
  case state of
    Just LabelState{ size } -> do
      glBlendFunc GL_ZERO GL_SRC_COLOR

      bind @Framebuffer Nothing

      let bounds = Interval offset (offset + size)
      viewport $ scale *^ bounds
      scissor  $ scale *^ bounds

      case bcolour of
        Just colour -> do
          setClearColour colour
          glClear GL_COLOR_BUFFER_BIT
        _ -> pure ()

      use textP $ do
        let textureUnit = TextureUnit 0
        setActiveTexture textureUnit
        bind (Just texture)

        set Text.U
          { sampler = Just textureUnit
          , colour  = Just transparent
          }

        bindArray quadA $ do
          let range = Interval 0 4
          drawArrays TriangleStrip range

          when (opaque colour /= black) $ do
            glBlendFunc GL_ONE GL_ONE
            set Text.U
              { sampler = Nothing
              , colour  = Just colour
              }
            drawArrays TriangleStrip range
    _ -> pure ()
