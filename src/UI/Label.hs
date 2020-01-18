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
import           Control.Effect.Lens ((?=))
import           Control.Effect.Lift
import           Control.Effect.Trace
import           Control.Lens ((^.))
import           Control.Monad (when)
import           Control.Monad.IO.Class.Lift
import           Data.Coerce
import           Data.Foldable (for_)
import           Data.Functor.I
import           Data.Functor.Interval as Interval
import           Data.IORef
import           Data.Word
import           GHC.Stack
import           GL.Array
import           GL.Buffer
import           GL.Effect.Check
import           GL.Framebuffer as GL
import           GL.Object
import           GL.Program
import           GL.Texture
import           GL.TextureUnit
import           GL.Viewport
import           Graphics.GL.Core41
import           Linear.Exts
import           UI.Colour
import           UI.Drawable
import           UI.Glyph (Instance(..), Run(..))
import qualified UI.Label.Glyph as Glyph
import qualified UI.Label.Text as Text
import           UI.Typeface
import qualified UI.Window as Window

data Label = Label
  { text    :: !(Drawable Text.U Text.V Text.Frag)
  , texture :: !(Texture 'Texture2D)
  , fbuffer :: !Framebuffer
  , ratio   :: !(Window.Pixels Int)
  , indices :: !(Buffer 'ElementArray Word32)
  , ref     :: !(IORef (Maybe LabelState))
  }

data LabelState = LabelState
  { size     :: !(V2 (Window.Pixels Int))
  , string   :: !String
  , baseline :: !(Window.Pixels Int)
  }


label
  :: ( Has Check sig m
     , Has Finally sig m
     , Has (Lift IO) sig m
     , Has (Reader Window.Window) sig m
     , Has Trace sig m
     , HasCallStack
     , Effect sig
     )
  => m Label
label = do
  texture <- gen1 @(Texture 'Texture2D)
  fbuffer <- gen1
  indices <- gen1

  program <- build Text.shader

  (buffer, array) <- load $ coerce @[V2 Float]
    [ V2 (-1) (-1)
    , V2   1  (-1)
    , V2 (-1)   1
    , V2   1    1
    ]

  ratio <- Window.ratio

  ref <- sendIO (newIORef Nothing)
  pure Label{ text = Drawable{ program, array, buffer }, texture, fbuffer, ref, ratio, indices }

labelSize :: Has (Lift IO) sig m => Label -> m (V2 (Window.Pixels Int))
labelSize = sendM . fmap (maybe (V2 0 0) UI.Label.size) . readIORef . ref


-- | Set the label’s text.
setLabel :: (HasCallStack, Has Check sig m, Has (Lift IO) sig m) => Label -> Font -> String -> m ()
setLabel Label{ texture, fbuffer, ratio, ref } font@(Font face _) string
  | null string = sendM (writeIORef ref Nothing)
  | otherwise   = runLiftIO $ do
    state <- sendIO (readIORef ref)
    case state of
      Just LabelState{ string = oldString } | string == oldString -> pure ()
      _ -> do
        glBlendFunc GL_ONE GL_ONE -- add

        Run instances b <- layoutString face string

        let b' = Interval (pure floor) (pure ceiling) <*> fontScale font *^ (fromIntegral <$> b)
            size = Interval.size b'
            baseline = b'^.min_._y

        bind (Just texture)
        setParameter Texture2D MagFilter Nearest
        setParameter Texture2D MinFilter Nearest
        setParameter Texture2D WrapS ClampToEdge
        setParameter Texture2D WrapT ClampToEdge
        setImageFormat Texture2D RGBA8 (ratio *^ size) RGBA (Packed8888 True)

        bind (Just fbuffer)
        attachTexture (GL.Colour 0) texture

        viewport $ ratio *^ Interval 0 size
        scissor  $ ratio *^ Interval 0 size

        setClearColour (transparent :: Colour Double)
        glClear GL_COLOR_BUFFER_BIT

        let V2 sx sy = fromIntegral ratio / fmap fromIntegral size
        runReader face . using glyphs $ do
          Glyph.ratio_     ?= ratio
          Glyph.fontScale_ ?= fontScale font
          Glyph.matrix_
            ?=  translated (-1)
            !*! scaled     (V3 sx sy 1)
            !*! translated (fromIntegral <$> negated (min' b'))
          -- FIXME: draw elements
          for_ instances $ \ Instance{ offset, range } -> do
            Glyph.offset_ ?= offset
            drawArraysInstanced Triangles range 6

        sendIO (writeIORef ref (Just LabelState{ UI.Label.size, string, baseline }))


drawLabel
  :: ( HasCallStack
     , Has Check sig m
     , Has (Lift IO) sig m
     )
  => Label
  -> V2 (Window.Pixels Int)
  -> Colour Float
  -> Maybe (Colour Float)
  -> m ()
drawLabel label@Label{ texture, ratio, ref } offset colour bcolour = runReader label . runLiftIO $ do
  state <- sendIO (readIORef ref)
  case state of
    Just LabelState{ size, baseline } -> do
      glBlendFunc GL_ZERO GL_SRC_COLOR

      bind @Framebuffer Nothing

      let offset' = offset + V2 0 baseline
          bounds = Interval offset' (offset' + size)
      viewport $ ratio *^ bounds
      scissor  $ ratio *^ bounds

      case bcolour of
        Just colour -> do
          setClearColour colour
          glClear GL_COLOR_BUFFER_BIT
        _ -> pure ()

      using text $ do
        let textureUnit = TextureUnit 0
        setActiveTexture textureUnit
        bind (Just texture)

        Text.sampler_ ?= textureUnit
        Text.colour_  ?= transparent

        let range = Interval 0 4
        drawArrays TriangleStrip range

        when (opaque colour /= black) $ do
          glBlendFunc GL_ONE GL_ONE
          Text.colour_ ?= colour
          drawArrays TriangleStrip range
    _ -> pure ()
