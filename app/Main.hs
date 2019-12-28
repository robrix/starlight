{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module Main
( main
, distanceScale
) where

import           Control.Applicative (liftA2)
import           Control.Carrier.Empty.Maybe
import           Control.Carrier.Finally
import           Control.Carrier.Profile.Time
import           Control.Carrier.Reader
import           Control.Carrier.State.Strict
import           Control.Carrier.Time
import           Control.Effect.Lens ((%=), (*=), (+=), (-=), (.=))
import qualified Control.Effect.Lens as Lens
import           Control.Effect.Lift
import qualified Control.Exception.Lift as E
import           Control.Monad ((<=<), when)
import           Control.Monad.IO.Class.Lift (runLiftIO)
import           Data.Bool (bool)
import           Data.Coerce
import           Data.Foldable (for_)
import           Data.Function (fix, (&))
import           Data.Functor.Const
import           Data.Functor.Identity
import           Data.Interval
import qualified Data.Map as Map
import           Data.Maybe (isJust)
import           Data.Text (unpack)
import           Data.Time.Clock (UTCTime)
import           Geometry.Circle
import           Geometry.Rect
import           GHC.Stack
import           GL.Array
import           GL.Framebuffer
import           GL.Program
import           Graphics.GL.Core41
import           Lens.Micro (Lens', lens, (.~), (^.))
import           Linear.Affine
import           Linear.Exts
import           Linear.Matrix
import           Linear.Metric
import           Linear.Quaternion
import           Linear.V2 as Linear
import           Linear.V3 as Linear
import           Linear.V4
import           Linear.Vector as Linear
import           Physics.Delta
import qualified SDL
import qualified Starlight.Body as S
import           Starlight.Input
import qualified Starlight.Shader.Body as Body
import qualified Starlight.Shader.Radar as Radar
import qualified Starlight.Shader.Ship as Ship
import qualified Starlight.Shader.Stars as Stars
import qualified Starlight.Sol as S
import           System.FilePath
import qualified UI.Carrier.Window as Window
import           UI.Colour
import           UI.Font as Font
import           UI.Label as Label
import           Unit.Angle
import           Unit.Length
import           Unit.Mass
import           Unit.Time

main :: HasCallStack => IO ()
main = E.handle (putStrLn . E.displayException @E.SomeException) $ reportTimings . fst <=< runProfile $ do
  font <- readFontOfSize ("fonts" </> "DejaVuSans.ttf") 36

  Window.runWindow "Starlight" (V2 1024 768) . runFinally . runTime $ now >>= \ start ->
    evalState @Input mempty
    . evalState GameState
      { throttle = 20
      , position = P (V2 25000 0)
      , velocity = V2 0 75
      , rotation = axisAngle (unit _z) (pi/2)
      , target   = Nothing
      }
    . evalState start
    . runReader S.system $ do
      starsP <- build Stars.shader
      shipP  <- build Ship.shader
      radarP <- build Radar.shader
      bodyP  <- build Body.shader

      label <- Label.label font white
      prepareLabel label $ ['0'..'9'] <> ['a'..'z'] <> ['A'..'Z'] <> "./"

      quadA   <- load quadV
      shipA   <- load shipV
      circleA <- load circleV
      radarA  <- load radarV

      glEnable GL_BLEND
      glEnable GL_DEPTH_CLAMP
      glEnable GL_SCISSOR_TEST
      glEnable GL_PROGRAM_POINT_SIZE

      fix $ \ loop -> do
        t <- realToFrac <$> since start
        system <- ask
        let bodies = S.bodiesAt system systemTrans (getDelta t)
        continue <- fmap isJust . runEmpty $
          input >> controls bodies label >>= physics bodies >>= draw DrawState{ quadA, shipA, circleA, radarA, starsP, shipP, radarP, bodyP, label } bodies
        when continue $ do
          Window.swap
          loop

reportTimings :: Has (Lift IO) sig m => Timings -> m ()
reportTimings (Timings ts) = for_ (Map.toList ts) $ \ (l, t) -> sendM $ do
  putStrLn $ unpack l <> ": " <> showTiming t where
  showTiming t = "{min: " <> show (min' t) <> ", mean: " <> show (mean t) <> ", max: " <> show (max' t) <> "}"


distanceScale :: Float
distanceScale = 10000 / getMetres (S.radius S.sol)

systemTrans :: M44 Float
systemTrans = scaled (V4 distanceScale distanceScale distanceScale 1) -- scale solar system distances down


shipV :: [Ship.V Identity]
shipV = coerce @[V2 Float]
  [ V2 1      0
  , V2 0      (-0.5)
  , V2 (-0.5) 0
  , V2 0      0.5
  ]

quadV :: [Stars.V Identity]
quadV = coerce @[V2 Float]
  [ V2 (-1) (-1)
  , V2   1  (-1)
  , V2 (-1)   1
  , V2   1    1
  ]

circleV :: [Body.V Identity]
circleV = coerce @[V4 Float] . map (`ext` V2 0 1) $ circle 1 128

radarV :: [Radar.V Identity]
radarV = coerce @[Float] [ fromIntegral t / fromIntegral n | t <- [-n..n] ] where
  n = (16 :: Int)

controls
  :: ( Has (Lift IO) sig m
     , Has (State Input) sig m
     , Has (State GameState) sig m
     , Has (State UTCTime) sig m
     , Has Time sig m
     )
  => [S.Instant Float]
  -> Label
  -> m (Delta Seconds Float)
controls bodies label = do
  Delta (Seconds dt) <- fmap realToFrac . since =<< get
  put =<< now

  whenM (Lens.use (_pressed SDL.KeycodePlus `or` _pressed SDL.KeycodeEquals)) $
    _throttle += dt * 10
  whenM (Lens.use (_pressed SDL.KeycodeMinus)) $
    _throttle -= dt * 10

  thrust <- (dt *) <$> Lens.use _throttle

  let angular = dt *^ Radians 5

  whenM (Lens.use (_pressed SDL.KeycodeUp)) $ do
    rotation <- Lens.use _rotation
    _velocity += rotate rotation (unit _x ^* thrust) ^. _xy
  whenM (Lens.use (_pressed SDL.KeycodeDown)) $ do
    rotation <- Lens.use _rotation
    velocity <- Lens.use _velocity
    let angle = fst (polar2 (negated velocity))
        proposed = axisAngle (unit _z) (getRadians angle)
        delta = abs (wrap (Interval (-pi) pi) (snd (toAxisAngle rotation) - angle))
    _rotation .= slerp rotation proposed (min 1 (getRadians (angular / delta)))

  whenM (Lens.use (_pressed SDL.KeycodeLeft)) $
    _rotation *= axisAngle (unit _z) (getRadians angular)
  whenM (Lens.use (_pressed SDL.KeycodeRight)) $
    _rotation *= axisAngle (unit _z) (getRadians (-angular))

  whenM (Lens.use (_pressed SDL.KeycodeTab)) $ do
    shift <- Lens.use (_pressed SDL.KeycodeLShift `or` _pressed SDL.KeycodeRShift)
    _target %= switchTarget shift
    _pressed SDL.KeycodeTab .= False

  setLabel label (show (round (dt * 1000) :: Int) <> "ms/" <> show (roundToPlaces 1 (1/dt)) <> "fps")

  pure (Delta (Seconds dt)) where
  switchTarget = \case
    False -> maybe (Just 0) (\ i -> i + 1 <$ guard (i + 1 < length bodies))
    True  -> maybe (Just (pred (length bodies))) (\ i -> i - 1 <$ guard (i - 1 >= 0))
  whenM c t = c >>= bool (pure ()) t
  or = liftA2 (liftA2 (coerce (||)))


physics
  :: Has (State GameState) sig m
  => [S.Instant Float]
  -> Delta Seconds Float
  -> m GameState
physics bodies (Delta (Seconds dt)) = do
  for_ bodies applyGravity

  s@GameState { velocity } <- get
  _position += P velocity
  pure s where
  applyGravity S.Instant { transform, body = S.Body { mass }} = do
    P position <- Lens.use _position
    let pos = (transform !* V4 0 0 0 1) ^. _xy
        r = qd pos position
        bigG = 6.67430e-11
    _velocity += dt * bigG * distanceScale ** 2 * getKilograms mass / r *^ normalize (pos ^-^ position)


-- | Compute the zoom factor for the given velocity.
--
-- Higher values correlate to more of the scene being visible.
zoomForSpeed :: V2 Int -> Float -> Float
zoomForSpeed size x
  | x < min_ speed = min_ zoom
  | x > max_ speed = max_ zoom
  | otherwise      = fromUnit zoom (easeInOutCubic (toUnit speed x)) where
  zoom = Interval 1 6
  speed = speedAt <$> zoom
  speedAt x = x / 25 * fromIntegral (maximum size)

easeInOutCubic :: Float -> Float
easeInOutCubic t
  | t < 0.5   = 4 * t ** 3
  | otherwise = (t - 1) * (2 * t - 2) ** 2 + 1

draw
  :: ( Has Finally sig m
     , Has (Lift IO) sig m
     , Has Profile sig m
     , Has Window.Window sig m
     )
  => DrawState
  -> [S.Instant Float]
  -> GameState
  -> m ()
draw DrawState{ quadA, circleA, shipA, radarA, shipP, starsP, radarP, bodyP, label } bodies GameState{ position, velocity, rotation, target } = measure "draw" . runLiftIO $ do
  bind @Framebuffer Nothing

  scale <- Window.scale
  size <- Window.size
  viewport $ scale *^ Rect 0 size
  scissor  $ scale *^ Rect 0 size

  glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA

  let zoomOut = zoomForSpeed size (norm velocity)

  measure "stars" . use starsP $ do
    scale <- Window.scale
    size <- Window.size
    set Stars.U
      { resolution = Just (size ^* scale)
      , origin     = Just (position / P size)
      , zoom       = Just zoomOut
      }

    bindArray quadA $
      drawArrays TriangleStrip (Interval 0 4)

  scale <- Window.scale
  size <- Window.size
  let V2 sx sy = scale / size ^* (1 / zoomOut)
      window = scaled (V4 sx sy 1 1) -- transform the [[-1,1], [-1,1]] interval to window coordinates

  measure "ship" . use shipP $ do
    set Ship.U
      { matrix = Just
          $   window
          !*! scaled (V4 15 15 15 1)
          !*! mkTransformation rotation 0
      , colour = Just white
      }

    bindArray shipA $
      drawArrays LineLoop (Interval 0 4)

  let origin = window !*! translated3 (ext (negated (unP position)) 0)
      drawBody S.Instant{ body = S.Body{ radius = Metres r, colour }, transform, rotation } = do
        set Body.U
          { matrix = Just
              $   origin
              !*! transform
              !*! scaled (V4 r r r 1)
              !*! mkTransformation rotation 0
          , colour = Just colour
          }

        drawArraysInstanced LineLoop (Interval 0 (length circleV)) 3

  measure "bodies" $
    use bodyP . bindArray circleA $ origin `seq` for_ bodies drawBody

  measure "radar" . use radarP $ do
    let here = unP position
        n = 10 :: Int
        minSweep = 0.0133 -- at d=150, makes approx. 4px blips
        -- FIXME: skip blips for extremely distant objects
        drawBlip S.Instant{ body = S.Body { name, radius = Metres r, colour }, transform } = do
          let there = (transform !* V4 0 0 0 1) ^. _xy
              angle = angleTo here there
              d = distance here there
              direction = normalize (there ^-^ here)
              -- FIXME: apply easing so this works more like a spring
              step = max 1 (min (50 * zoomOut) (d / fromIntegral n))
              drawAtRadius radius minSweep colour = do
                let edge = distanceScale * r * (min d radius/d) *^ perp direction + direction ^* radius + here
                    sweep = max minSweep (abs (wrap (Interval (-pi) pi) (angleTo here edge - angle)))

                set Radar.U
                  { matrix = Just (scaled (V3 sx sy 1))
                  , radius = Just radius
                  , angle  = Just angle
                  , sweep  = Just sweep
                  , colour = Just colour
                  }

                drawArrays LineStrip (Interval 0 (length radarV))

          drawAtRadius 100 minSweep (colour & _a .~ 0.5)

          when (Just name == (target >>= \ i -> S.name (S.body (bodies !! i)) <$ guard (i < length bodies))) $ for_ [1..n] $ \ i ->
            drawAtRadius (step * fromIntegral i) (minSweep * Radians (fromIntegral i / 7)) ((colour + 0.5 * fromIntegral i / fromIntegral n) ** 2 & _a .~ (fromIntegral i / fromIntegral n))

    bindArray radarA $ for_ bodies drawBlip

  measure "drawLabel" $ drawLabel label

roundToPlaces :: RealFloat a => Int -> a -> a
roundToPlaces n x = fromInteger (round (x * n')) / n' where
  n' = 10 ** fromIntegral n


data DrawState = DrawState
  { quadA   :: Array (Stars.V Identity)
  , circleA :: Array (Body.V  Identity)
  , shipA   :: Array (Ship.V  Identity)
  , radarA  :: Array (Radar.V Identity)
  , starsP  :: Program Stars.U Stars.V Stars.O
  , shipP   :: Program Ship.U  Ship.V  Ship.O
  , bodyP   :: Program Body.U  Body.V  Body.O
  , radarP  :: Program Radar.U Radar.V Radar.O
  , label   :: Label
  }


data GameState = GameState
  { throttle :: !Float
  , position :: !(Point V2 Float)
  , velocity :: !(V2 Float)
  , rotation :: !(Quaternion Float)
  , target   :: !(Maybe Int)
  }
  deriving (Show)

_throttle :: Lens' GameState Float
_throttle = lens throttle (\ s v -> s { throttle = v })

_position :: Lens' GameState (Point V2 Float)
_position = lens position (\ s v -> s { position = v })

_velocity :: Lens' GameState (V2 Float)
_velocity = lens velocity (\ s v -> s { velocity = v })

_rotation :: Lens' GameState (Quaternion Float)
_rotation = lens rotation (\ s r -> s { rotation = r })

_target :: Lens' GameState (Maybe Int)
_target = lens target (\ s t -> s { target = t })
