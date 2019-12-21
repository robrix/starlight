{-# LANGUAGE DataKinds, DisambiguateRecordFields, FlexibleContexts, GeneralizedNewtypeDeriving, NamedFieldPuns, OverloadedStrings, TypeApplications, TypeOperators #-}
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
import Data.Foldable (for_)
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
import Linear.Metric
import Linear.V (Size)
import Linear.V2 as Linear
import Linear.V3 as Linear
import Linear.V4 as Linear
import Linear.Vector as Linear
import Physics.Delta
import qualified SDL
import qualified Starlight.Body as S
import qualified UI.Carrier.Window as Window
import UI.Colour
import UI.Font as Font
import UI.Label
import Unit.Angle
import Unit.Length
import Unit.Mass
import Unit.Time

main :: HasCallStack => IO ()
main = E.handle (putStrLn . E.displayException @E.SomeException) $ do
  tahoma <- readFontOfSize "/System/Library/Fonts/Supplemental/Tahoma.ttf" 36
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

  Window.runWindow "Starlight" (V2 1024 768) . runFinally . runTime $ now >>= \ start ->
    runProgram
    . evalState @Input mempty
    . evalState PlayerState
      { position = P (V2 700 0)
      , velocity = Delta (P (V2 0 14))
      , rotation = pi/2
      }
    . evalState start $ do
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
            dt <- fmap (getSeconds . getDelta . realToFrac) . since =<< get

            let thrust  = dt *  1
                angular = dt *^ pi

            when (pressed SDL.KeycodeUp   input) $ do
              rotation <- Lens.use _rotation
              _velocity += Delta (P (cartesian2 rotation thrust))
            when (pressed SDL.KeycodeDown input) $ do
              rotation <- Lens.use _rotation
              velocity <- Lens.use _velocity
              let angle = fst (polar2 (negated (unP (getDelta velocity))))
                  delta = wrap $ rotation - angle
                  (+-=) = if delta < 0 then (+=) else (-=)
              _rotation +-= min angular (abs delta)

            when (pressed SDL.KeycodeLeft  input) $
              _rotation += angular
            when (pressed SDL.KeycodeRight input) $
              _rotation -= angular

            t <- getSeconds . getDelta . realToFrac <$> since start

            let distanceScale = 0.000000718907261
                applyGravity rel S.Body { mass, orbit, satellites } = do
                  P position <- Lens.use _position
                  let pos = rel + distanceScale *^ uncurry cartesian2 (S.position orbit (t * 86400))
                      r = qd pos position
                  _velocity += Delta (P (((0.0000000000000000001 * distanceScale * getKilograms mass) / r) *^ normalize (pos ^-^ position)))
                  for_ satellites (applyGravity pos)

            applyGravity 0 S.sol

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
                window
                  =   scaled (V3 sx sy 1)
                  !*! translated (negated (unP position))

            use ship $ do
              set @"colour" $ V4 1 1 1 1
              set @"matrix3"
                $   window
                !*! translated (unP position)
                !*! scaled     (V3 25 25 1)
                !*! rotated    (getRadians rotation)

              drawArrays LineLoop (Range 0 4)

              let drawBody rel S.Body { radius = Metres r, colour, orbit, satellites } = do
                    let pos = rel + uncurry cartesian2 (S.position orbit (t * 86400))
                    set @"colour" colour
                    set @"matrix3"
                      $   window
                      !*! translated (distanceScale *^ pos)
                      !*! scaled     (V3 (r * distanceScale) (r * distanceScale) 1)

                    drawArrays LineLoop (Range 0 (length starVertices))

                    for_ satellites (drawBody pos)

              bind (Just starArray)
              drawBody 0 S.sol

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
_rotation = lens rotation (\ s r -> s { rotation = wrap r })


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
