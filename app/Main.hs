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
import Data.Interval
import Data.Maybe (isJust)
import Data.Time.Clock (UTCTime)
import Geometry.Circle
import Geometry.Rect
import GHC.Stack
import GL.Array
import GL.Carrier.Program.Live
import GL.Framebuffer as GL
import qualified GL.Program as GL
import GL.Shader
import Graphics.GL.Core41
import Lens.Micro (Lens', (^.), lens)
import Linear.Affine
import Linear.Exts
import Linear.Matrix
import Linear.Metric
import Linear.V2 as Linear
import Linear.V3 as Linear
import Linear.V4 as Linear
import Linear.Vector as Linear
import Physics.Delta
import qualified SDL
import qualified Starlight.Body as S
import Starlight.Input
import System.FilePath
import qualified UI.Carrier.Window as Window
import UI.Colour
import UI.Font as Font
import UI.Label as Label
import Unit.Angle
import Unit.Length
import Unit.Mass
import Unit.Time

main :: HasCallStack => IO ()
main = E.handle (putStrLn . E.displayException @E.SomeException) $ do
  font <- readFontOfSize ("fonts" </> "DejaVuSans.ttf") 36

  Window.runWindow "Starlight" (V2 1024 768) . runFinally . runTime $ now >>= \ start ->
    runProgram
    . evalState @Input mempty
    . evalState PlayerState
      { position = P (V2 1700 0)
      , velocity = Delta (P (V2 0 20))
      , rotation = pi/2
      }
    . evalState start $ do
      starsP <- build
        [(Vertex, "src" </> "stars-vertex.glsl"), (Fragment, "src" </> "stars-fragment.glsl")]
      shipP <- build
        [(Vertex, "src" </> "ship-vertex.glsl"), (Fragment, "src" </> "ship-fragment.glsl")]

      label <- label

      quadA   <- loadVertices quadV
      shipA   <- loadVertices shipV
      circleA <- loadVertices circleV

      glEnable GL_BLEND
      glEnable GL_SCISSOR_TEST
      glEnable GL_PROGRAM_POINT_SIZE

      label <- setLabel label { Label.colour = white } font "hello"
      let drawState = DrawState { quadA, shipA, circleA, starsP, shipP }

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

shipV :: [V3 Float]
shipV =
  [ V3 1      0      0
  , V3 0      (-0.5) 0
  , V3 (-0.5) 0      0
  , V3 0      0.5    0
  ]

quadV :: [V2 Float]
quadV =
  [ V2 (-1) (-1)
  , V2   1  (-1)
  , V2 (-1)   1
  , V2   1    1
  ]

circleV :: [V2 Float]
circleV = circle 1 32

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


-- | Compute the zoom factor for the given velocity.
--
-- Higher values correlate to more of the scene being visible.
zoomForSpeed :: V2 Int -> Float -> Float
zoomForSpeed size x
  | x < min_ speed = min_ zoom
  | x > max_ speed = max_ zoom
  | otherwise        = fromUnit zoom (easeInOutCubic (toUnit speed x)) where
  zoom = Interval 1 4
  speed = speedAt <$> zoom
  bound = fromIntegral (min (size ^. _x) (size ^. _y))
  speedAt x = x / 100 * bound

easeInOutCubic :: Float -> Float
easeInOutCubic t
  | t < 0.5   = 4 * t ** 3
  | otherwise = (t - 1) * (2 * t - 2) ** 2 + 1

draw
  :: ( Has (Lift IO) sig m
     , Has Program sig m
     , Has Window.Window sig m
     )
  => DrawState
  -> Delta Seconds Float
  -> PlayerState
  -> m ()
draw DrawState { quadA, circleA, shipA, shipP, starsP } t PlayerState { position, velocity, rotation } = runLiftIO $ do
  bind @Framebuffer Nothing

  scale <- Window.scale
  size <- Window.size
  viewport $ scale *^ Rect 0 size
  scissor  $ scale *^ Rect 0 size

  setClearColour black
  glClear GL_COLOR_BUFFER_BIT

  bind (Just quadA)

  let zoomOut = zoomForSpeed size (norm velocity)

  use starsP $ do
    scale <- Window.scale
    size <- Window.size
    set @"resolution" (size ^* scale)
    set @"origin" (position / P size)
    set @"zoom" zoomOut

    drawArrays TriangleStrip (Interval 0 4)

  bind (Just shipA)

  scale <- Window.scale
  size <- Window.size
  let V2 sx sy = scale / size ^* (1 / zoomOut)
      window
        =   scaled (V3 sx sy 1)
        !*! translated (negated (unP position))

  use shipP $ do
    set @"colour" $ V4 1 1 1 1
    set @"matrix"
      $   window
      !*! translated (unP position)
      !*! scaled     (V3 25 25 1)
      !*! rotated    rotation

    drawArrays LineLoop (Interval 0 4)

    let drawBody rel S.Body { radius = Metres r, colour, orbit, satellites } = do
          let trans = rel !*! S.transform orbit (getDelta t * 86400)
          set @"colour" colour
          set @"matrix"
            $   window
            !*! trans
            !*! scaled (V3 r r 1)

          drawArrays LineLoop (Interval 0 (length circleV))

          for_ satellites (drawBody trans)

    bind (Just circleA)
    drawBody (scaled (V3 distanceScale distanceScale 1)) S.sol

    let drawBlip rel S.Body { radius = Metres r, colour, orbit, satellites } = do
          let trans = rel !*! S.transform orbit (getDelta t * 86400)
              v2 = unP position
              v1 = (trans !* V3 0 0 1) ^. _xy
              angle = Radians (atan2 (v2 ^. _y) (v2 ^. _x) - atan2 (v1 ^. _y) (v1 ^. _x))
              d = distance v2 v1
          set @"colour" $ colour
          set @"matrix"
            $   window
            !*! translated (unP position)
            !*! translated (cartesian2 (angle - pi) (min d 150))
            !*! scaled     (V3 distanceScale distanceScale 1)
            !*! scaled     (V3 (min d 150/d) (min d 150/d) 1)
            !*! scaled     (V3 r r 1)

          drawArrays LineLoop (Interval 0 (length circleV))

          for_ satellites (drawBlip trans)

    drawBlip (scaled (V3 distanceScale distanceScale 1)) S.sol


data DrawState = DrawState
  { quadA   :: Array (V2 Float)
  , circleA :: Array (V2 Float)
  , shipA   :: Array (V3 Float)
  , starsP  :: GL.Program
    '[ "resolution" '::: V2 Float
     , "origin"     '::: Point V2 Float
     , "zoom"       '::: Float ]
  , shipP    :: GL.Program
    '[ "colour"     '::: V4 Float
     , "matrix"     '::: M33 Float ]
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
