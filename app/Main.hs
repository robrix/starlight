{-# LANGUAGE DataKinds, FlexibleContexts, GeneralizedNewtypeDeriving, NamedFieldPuns, OverloadedStrings, TypeApplications, TypeOperators #-}
module Main
( main
) where

import Control.Carrier.Empty.Maybe
import Control.Carrier.Finally
import Control.Carrier.State.Strict
import Control.Carrier.Time
import Control.Effect.Lens ((+=), (-=))
import qualified Control.Effect.Lens as Lens
import Control.Effect.Lift
import qualified Control.Exception.Lift as E
import Control.Monad (when)
import Data.Coerce (coerce)
import Data.Function (fix)
import Data.Maybe (isJust)
import qualified Data.IntSet as IntSet
import Foreign.Storable (Storable)
import Geometry.Circle
import Geometry.Rect
import GHC.Stack
import GHC.TypeLits
import GL.Array
import GL.Buffer
import GL.Carrier.Program.Live
import GL.Framebuffer as GL
import GL.Object
import GL.Range
import GL.Scalar
import GL.Shader
import Graphics.GL.Core41
import Lens.Micro (Lens', lens)
import Linear.Affine
import Linear.Exts
import Linear.Matrix
import Linear.V (Size)
import Linear.V2 as Linear
import Linear.V3 as Linear
import Linear.V4 as Linear
import Linear.Vector as Linear
import Physics.Delta
import Physics.Radians
import Physics.Seconds
import qualified SDL
import qualified UI.Carrier.Window as Window
import UI.Colour
import UI.Font as Font
import UI.Label

main :: HasCallStack => IO ()
main = E.handle (\ e -> putStrLn $ E.displayException @E.SomeException e) $ do
  tahoma <- readFontOfSize "/System/Library/Fonts/Supplemental/Tahoma.ttf" 288
  let shipVertices =
        [ V3 1      0      0
        , V3 0      (-0.5) 0
        , V3 (-0.5) 0      0
        , V3 0      0.5    0 :: V3 Float
        ]
      quadVertices =
        [ V2 (-1) (-1)
        , V2   1  (-1)
        , V2 (-1)   1
        , V2   1    1  :: V2 Float
        ]
      starVertices = circle 1 32

  Window.runWindow "Starlight" (V2 1024 768)
    . runFinally
    . runTime
    . runProgram
    . evalState @Input mempty
    . evalState PlayerState
      { position = 0
      , velocity = 0
      , rotation = 0
      }
    $ (\ m -> now >>= \ now -> evalState now m)
    $ do
      stars <- build
        @'[ "resolution" '::: V2 Float
          , "origin"     '::: Point V2 Float ]
        [(Vertex, "stars-vertex.glsl"), (Fragment, "stars-fragment.glsl")]
      ship <- build
        @'[ "colour"  '::: V4 Float
          , "matrix3" '::: M33 Float ]
        [(Vertex, "ship-vertex.glsl"), (Fragment, "ship-fragment.glsl")]

      label <- label

      (_, quadArray) <- loadVertices quadVertices
      (_, shipArray) <- loadVertices shipVertices
      (_, starArray) <- loadVertices starVertices

      glEnable GL_BLEND
      glEnable GL_SCISSOR_TEST

      label <- setLabel label { colour = white } tahoma "hello"

      let drawCanvas = do
            bind @Framebuffer Nothing

            scale <- Window.scale
            rect <- Rect 0 <$> Window.size
            viewport $ scale *^ rect
            scissor  $ scale *^ rect

            setClearColour black
            glClear GL_COLOR_BUFFER_BIT

            input <- input
            t <- fmap (getSeconds . getDelta . realToFrac) . since =<< get

            let thrust  = t *  1
                angular = t *^ pi

            when (pressed SDL.KeycodeUp   input) $ do
              rotation <- Lens.use _rotation
              _velocity += Delta (P (cartesian2 rotation thrust))
            when (pressed SDL.KeycodeDown input) $ do
              rotation <- Lens.use _rotation
              velocity <- Lens.use _velocity
              let angle = fst (polar2 (negated (unP (getDelta velocity))))
                  delta = rotation - angle
                  (+-=) = if delta < 0 then (+=) else (-=)
              _rotation +-= min angular (abs delta)

            when (pressed SDL.KeycodeLeft  input) $
              _rotation += angular
            when (pressed SDL.KeycodeRight input) $
              _rotation -= angular

            PlayerState
              { position
              , velocity
              , rotation } <- get

            bind (Just quadArray)

            use stars $ do
              scale <- Window.scale
              size <- Window.size
              set @"resolution" (size ^* (1 / scale))
              set @"origin" position

              drawArrays TriangleStrip (Range 0 4)

            bind (Just shipArray)

            scale <- Window.scale
            size <- Window.size
            let V2 sx sy = scale / size
                window = scaled (V3 sx sy 1)

            use ship $ do
              set @"colour" $ V4 1 1 1 1
              set @"matrix3"
                $   window
                !*! translated 0
                !*! scaled     (V3 50 50 1)
                !*! rotated    (getRadians rotation)

              drawArrays LineLoop (Range 0 4)

              bind (Just starArray)
              set @"matrix3"
                $   window
                !*! translated (negated (unP position))
                !*! scaled     (V3 50 50 1)
                !*! rotated    0

              drawArrays LineLoop (Range 0 (length starVertices))

            _position += getDelta velocity

      fix $ \ loop -> do
        continue <- isJust <$> runEmpty drawCanvas
        drawLabel label
        put =<< now
        when continue $
          Window.swap >> loop


data PlayerState = PlayerState
  { position :: !(Point V2 Float)
  , velocity :: !(Delta (Point V2) Float)
  , rotation :: !(Radians Float)
  }
  deriving (Eq, Ord, Show)

_position :: Lens' PlayerState (Point V2 Float)
_position = lens position (\ s v -> s { position = v })

_velocity :: Lens' PlayerState (Delta (Point V2) Float)
_velocity = lens velocity (\ s v -> s { velocity = v })

_rotation :: Lens' PlayerState (Radians Float)
_rotation = lens rotation (\ s r -> s { rotation = r })


input
  :: ( Has Empty sig m
     , Has (State Input) sig m
     , Has Window.Window sig m
     )
  => m Input
input = Window.input go >> get where
  go (SDL.Event _ p) = case p of
    SDL.QuitEvent -> empty
    SDL.KeyboardEvent (SDL.KeyboardEventData _ p _ (SDL.Keysym _ kc _)) -> key p kc
    _ -> pure ()


newtype Input = Input { unInput :: IntSet.IntSet }
  deriving (Monoid, Semigroup)

key :: Has (State Input) sig m => SDL.InputMotion -> SDL.Keycode -> m ()
key m = modify @Input . coerce . case m of
  { SDL.Pressed  -> IntSet.insert
  ; SDL.Released -> IntSet.delete }
  . fromIntegral . SDL.unwrapKeycode

pressed :: SDL.Keycode -> Input -> Bool
pressed code = IntSet.member (fromIntegral (SDL.unwrapKeycode code)) . unInput


loadVertices :: (KnownNat (Size v), Storable (v n), Scalar n, Has Finally sig m, Has (Lift IO) sig m) => [v n] -> m (Buffer 'GL.Buffer.Array (v n), Array (v n))
loadVertices vertices = do
  buffer <- gen1
  array  <- gen1

  bind (Just buffer)
  realloc buffer (length vertices) Static Draw
  copy buffer 0 vertices

  bind (Just array)
  configureArray buffer array
  pure (buffer, array)
