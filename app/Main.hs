{-# LANGUAGE DataKinds, FlexibleContexts, GeneralizedNewtypeDeriving, NamedFieldPuns, OverloadedStrings, TypeApplications, TypeOperators #-}
module Main
( main
) where

import Control.Carrier.Empty.Maybe
import Control.Carrier.Finally
import Control.Carrier.State.Strict
import Control.Carrier.Time
import Control.Effect.Lens ((+=))
import qualified Control.Effect.Lens as Lens
import Control.Effect.Lift
import Data.Foldable
import Data.Function (fix)
import Data.Time.Clock (UTCTime)
import Foreign.Storable (Storable)
import Geometry.Rect
import GHC.Stack
import GHC.TypeLits
import GL.Array
import GL.Buffer
import GL.Carrier.Program.Live
import GL.Framebuffer as GL
import GL.Object
import GL.Scalar
import GL.Shader
import Graphics.GL.Core41
import Lens.Micro (Lens', lens)
import Linear.Affine
import Linear.V (Size)
import Linear.V2 as Linear
import Linear.V3 as Linear
import Linear.V4 as Linear
import Linear.Vector as Linear
import Physics.Delta
import Physics.Radians
import Physics.Seconds
import qualified SDL
import UI.Colour
import UI.Font as Font
import UI.Layer
import qualified UI.Carrier.Window as Window

main :: HasCallStack => IO ()
main = do
  tahoma <- readFontOfSize "/System/Library/Fonts/Supplemental/Tahoma.ttf" 288
  let (shipVertices, shipRanges) = combineGeometry
        [ [ V3 1      0      0
          , V3 0      (-0.5) 0
          , V3 (-0.5) 0      0
          , V3 0      0.5    0 :: V3 Float
          ]
        ]
      screenQuadVertices =
        [ V2 (-1) (-1)
        , V2   1  (-1)
        , V2 (-1)   1
        , V2   1    1  :: V2 Float
        ]

  Window.runWindow "Starlight" (V2 1024 768)
    . runFinally
    . runTime
    . runProgram
    . evalState PlayerState { position = 0, velocity = 0, rotation = 0 }
    $ (\ m -> now >>= \ now -> evalState now m)
    $ do
      stars <- build
        @'[ "resolution" '::: V3 Float
          , "origin"     '::: Point V2 Float ]
        [(Vertex, "stars-vertex.glsl"), (Fragment, "stars-fragment.glsl")]
      ship <- build
        @'[ "colour"      '::: V4 Float
          , "translation" '::: V2 Float
          , "scale"       '::: V2 Float
          , "rotation"    '::: Radians Float ]
        [(Vertex, "ship-vertex.glsl"), (Fragment, "ship-fragment.glsl")]

      (_, screenQuadArray) <- loadVertices screenQuadVertices
      (_, shipArray)       <- loadVertices shipVertices

      glEnable GL_BLEND
      glEnable GL_SCISSOR_TEST

      let drawCanvas = do
            windowScale <- Window.scale
            windowSize <- Window.size
            let scale = windowScale / windowSize
                V2 width height = windowSize

            PlayerState { position, velocity, rotation } <- handleInput

            bind (Just screenQuadArray)

            use stars $ do
              set @"resolution" (V3 width height 8)
              set @"origin" position

              drawArrays TriangleStrip (Range 0 4)

            bind (Just shipArray)

            use ship $ do
              set @"colour" $ V4 1 1 1 1
              set @"translation" 0
              set @"scale" (scale * 50)
              set @"rotation" rotation

              traverse_ (drawArrays LineLoop) shipRanges

            _position += getDelta velocity

      fix $ \ loop -> do
        rect <- Rect 0 <$> Window.size
        res <- runEmpty $ drawLayer Nothing (Just black) rect drawCanvas
        put =<< now
        maybe (pure ()) (const (Window.swap >> loop)) res

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


handleInput
  :: ( Has Empty sig m
     , Has (State PlayerState) sig m
     , Has (State UTCTime) sig m
     , Has Time sig m
     , Has Window.Window sig m
     )
  => m PlayerState
handleInput = do
  t <- fmap (getSeconds . getDelta . realToFrac) . since =<< get

  let thrust  = t *  0.01
      angular = t *^ pi

  Window.input $ \ event -> case SDL.eventPayload event of
    SDL.QuitEvent -> empty
    SDL.KeyboardEvent (SDL.KeyboardEventData _ _ _ (SDL.Keysym _ kc _)) -> case kc of
      SDL.KeycodeUp    -> do
        rotation <- Lens.use _rotation
        _velocity += Delta (P (cartesian2 rotation thrust))
      SDL.KeycodeLeft  -> _rotation += angular
      SDL.KeycodeRight -> _rotation += -angular
      _                -> pure ()
    _ -> pure ()

  get


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
  array  <- gen1

  bind (Just buffer)
  realloc buffer (length vertices) Static Draw
  copy buffer 0 vertices

  bind (Just array)
  configureArray buffer array
  pure (buffer, array)
