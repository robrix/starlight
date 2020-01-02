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

import           Control.Carrier.Reader
import           Control.Effect.Finally
import           Control.Effect.Lens ((.=))
import           Control.Effect.Lift
import           Control.Monad (when)
import           Control.Monad.IO.Class.Lift
import           Data.Coerce
import           Data.Foldable (for_)
import           Data.Functor.Identity
import           Data.Functor.Interval as Interval
import           Data.IORef
import           GHC.Stack
import           GL.Array
import           GL.Framebuffer as GL
import           GL.Object
import           GL.Program
import           GL.Texture
import           GL.TextureUnit
import           GL.Viewport
import           Graphics.GL.Core41
import           Lens.Micro ((^.))
import           Linear.Exts
import           UI.Colour
import           UI.Drawable
import           UI.Glyph (Instance(..), Run(..))
import qualified UI.Label.Glyph as Glyph
import qualified UI.Label.Text as Text
import           UI.Typeface
import qualified UI.Window as Window

data Label = Label
  { text    :: !(Drawable Text.U  Text.V  Text.O)
  , texture :: !(Texture 'Texture2D)
  , fbuffer :: !Framebuffer
  , scale   :: !Int
  , ref     :: !(IORef (Maybe LabelState))
  }

data LabelState = LabelState
  { size     :: !(V2 Int)
  , string   :: !String
  , baseline :: !Int
  }


label
  :: ( Has Finally sig m
     , Has (Lift IO) sig m
     , Has (Reader Window.Window) sig m
     , HasCallStack
     )
  => m Label
label = do
  texture <- gen1 @(Texture 'Texture2D)
  fbuffer <- gen1

  program  <- build Text.shader

  array <- load $ coerce @[V2 Float]
    [ V2 (-1) (-1)
    , V2   1  (-1)
    , V2 (-1)   1
    , V2   1    1
    ]

  scale <- Window.scale

  ref <- sendIO (newIORef Nothing)
  pure Label{ text = Drawable{ program, array }, texture, fbuffer, ref, scale }

labelSize :: Has (Lift IO) sig m => Label -> m (V2 Int)
labelSize = sendM . fmap (maybe (V2 0 0) UI.Label.size) . readIORef . ref


-- | Set the label’s text.
setLabel :: (HasCallStack, Has (Lift IO) sig m) => Label -> Font -> String -> m ()
setLabel Label{ texture, fbuffer, scale, ref } font@(Font face _) string
  | null string = sendM (writeIORef ref Nothing)
  | otherwise   = runLiftIO $ do
    state <- sendIO (readIORef ref)
    case state of
      Just LabelState{ string = oldString } | string == oldString -> pure ()
      _ -> do
        glBlendFunc GL_ONE GL_ONE -- add

        Run instances b <- layoutString face string

        let b' = Interval (pure floor) (pure ceiling) <*> fontScale font *^ b
            size = Interval.size b'
            baseline = b' ^. _min . _y

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
        runReader face . using glyphs $ do
          Glyph.scale_     .= Just (1 / fromIntegral scale)
          Glyph.fontScale_ .= Just (fontScale font)
          Glyph.matrix_    .= Just
            (   translated (-1)
            !*! scaled     (V3 sx sy 1)
            !*! translated (fromIntegral <$> negated (min_ b')))
          for_ instances $ \ Instance{ offset, range } -> do
            Glyph.offset_ .= Just offset
            drawArraysInstanced Triangles range 6

        sendIO (writeIORef ref (Just LabelState{ UI.Label.size, string, baseline }))


drawLabel
  :: ( HasCallStack
     , Has (Lift IO) sig m
     )
  => Label
  -> V2 Int
  -> Colour Float
  -> Maybe (Colour Float)
  -> m ()
drawLabel label@Label{ texture, scale, ref } offset colour bcolour = runReader label . runLiftIO $ do
  state <- sendIO (readIORef ref)
  case state of
    Just LabelState{ size, baseline } -> do
      glBlendFunc GL_ZERO GL_SRC_COLOR

      bind @Framebuffer Nothing

      let offset' = offset + V2 0 baseline
          bounds = Interval offset' (offset' + size)
      viewport $ scale *^ bounds
      scissor  $ scale *^ bounds

      case bcolour of
        Just colour -> do
          setClearColour colour
          glClear GL_COLOR_BUFFER_BIT
        _ -> pure ()

      using text $ do
        let textureUnit = TextureUnit 0
        setActiveTexture textureUnit
        bind (Just texture)

        Text.sampler_ .= Just textureUnit
        Text.colour_  .= Just transparent

        let range = Interval 0 4
        drawArrays TriangleStrip range

        when (opaque colour /= black) $ do
          glBlendFunc GL_ONE GL_ONE
          Text.colour_ .= Just colour
          drawArrays TriangleStrip range
    _ -> pure ()
