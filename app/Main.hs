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
import Control.Monad.IO.Class.Lift (runLiftIO)
import Data.Foldable (for_)
import Data.Function (fix)
import Data.Maybe (isJust)
import Data.Time.Clock (UTCTime)
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
import qualified GL.Program as GL
import GL.Range
import GL.Scalar
import GL.Shader
import Graphics.GL.Core41
import Lens.Micro (Lens', (^.), lens)
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
import Starlight.Input
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

  Window.runWindow "Starlight" (V2 1024 768) . runFinally . runTime $ now >>= \ start ->
    runProgram
    . evalState @Input mempty
    . evalState PlayerState
      { position = P (V2 1700 0)
      , velocity = Delta (P (V2 0 20))
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

      quadArray <- loadVertices quadVertices
      shipArray <- loadVertices shipVertices
      starArray <- loadVertices starVertices

      glEnable GL_BLEND
      glEnable GL_SCISSOR_TEST

      label <- setLabel label { colour = white } tahoma "hello"
      let drawState = DrawState { quadArray, shipArray, starArray, stars, ship }

      fix $ \ loop -> do
        t <- realToFrac <$> since start
        continue <- fmap isJust . runEmpty $ do
          state <- physics t =<< input
          draw drawState t state
        drawLabel label
        put =<< now
        when continue $
          Window.swap >> loop


distanceScale :: Float
distanceScale = 0.000000718907261

shipVertices :: [V3 Float]
shipVertices =
  [ V3 1      0      0
  , V3 0      (-0.5) 0
  , V3 (-0.5) 0      0
  , V3 0      0.5    0 :: V3 Float
  ]

quadVertices :: [V2 Float]
quadVertices =
  [ V2 (-1) (-1)
  , V2   1  (-1)
  , V2 (-1)   1
  , V2   1    1  :: V2 Float
  ]

starVertices :: [V2 Float]
starVertices = circle 1 32

physics
  :: ( Has (State UTCTime) sig m
     , Has (State PlayerState) sig m
     , Has Time sig m
     )
  => Delta Seconds Float
  -> Input
  -> m PlayerState
physics t input = do
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

  let applyGravity rel S.Body { mass, orbit, satellites } = do
        P position <- Lens.use _position
        let trans = rel + S.transform orbit (getDelta t * 86400)
            pos = (trans !* V3 0 0 1) ^. _xy
            r = qd pos position
            bigG = 6.67430e-11
        _velocity += Delta (P (dt * bigG * distanceScale * distanceScale * getKilograms mass / r *^ normalize (pos ^-^ position)))
        for_ satellites (applyGravity trans)

  applyGravity (scaled (V3 distanceScale distanceScale 1)) S.sol

  s@PlayerState { velocity } <- get
  _position += getDelta velocity
  pure s

draw
  :: ( Has (Lift IO) sig m
     , Has Program sig m
     , Has Window.Window sig m
     )
  => DrawState
  -> Delta Seconds Float
  -> PlayerState
  -> m ()
draw DrawState { quadArray, starArray, shipArray, ship, stars } t PlayerState { position, rotation } = runLiftIO $ do
  bind @Framebuffer Nothing

  scale <- Window.scale
  rect <- Rect 0 <$> Window.size
  viewport $ scale *^ rect
  scissor  $ scale *^ rect

  setClearColour black
  glClear GL_COLOR_BUFFER_BIT

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
      !*! rotated    rotation

    drawArrays LineLoop (Range 0 4)

    let drawBody rel S.Body { radius = Metres r, colour, orbit, satellites } = do
          let trans = rel !*! S.transform orbit (getDelta t * 86400)
          set @"colour" colour
          set @"matrix3"
            $   window
            !*! trans
            !*! scaled (V3 r r 1)

          drawArrays LineLoop (Range 0 (length starVertices))

          for_ satellites (drawBody trans)

    bind (Just starArray)
    drawBody (scaled (V3 distanceScale distanceScale 1)) S.sol


data DrawState = DrawState
  { quadArray :: Array (V2 Float)
  , starArray :: Array (V2 Float)
  , shipArray :: Array (V3 Float)
  , stars     :: GL.Program
    '[ "resolution" '::: V2 Float
     , "origin"     '::: Point V2 Float ]
  , ship      :: GL.Program
    '[ "colour"  '::: V4 Float
     , "matrix3" '::: M33 Float ]
  }


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


loadVertices :: (KnownNat (Size v), Storable (v n), Scalar n, Has Finally sig m, Has (Lift IO) sig m) => [v n] -> m (Array (v n))
loadVertices vertices = do
  buffer <- gen1
  array  <- gen1

  bind (Just buffer)
  realloc buffer (length vertices) Static Draw
  copy buffer 0 vertices

  bind (Just array)
  array <$ configureArray buffer array
