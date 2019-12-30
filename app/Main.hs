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
) where

import           Control.Applicative (liftA2)
import           Control.Carrier.Empty.Maybe
import           Control.Carrier.Finally
import           Control.Carrier.Profile.Time
import           Control.Carrier.State.Strict
import           Control.Effect.Lens ((%=), (*=), (+=), (-=), (.=))
import qualified Control.Effect.Lens as Lens
import           Control.Effect.Lift
import qualified Control.Exception.Lift as E
import           Control.Monad ((<=<), when)
import           Control.Monad.IO.Class.Lift (runLiftIO)
import           Data.Coerce
import           Data.Foldable (foldl', for_)
import           Data.Function (fix, (&))
import           Data.Functor.Const
import           Data.Functor.I
import           Data.Functor.Interval
import           Data.List (sortOn)
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Map as Map
import           Data.Maybe (isJust)
import           Data.Ord (Down(..))
import           Data.Text (unpack)
import           Data.Time.Clock (NominalDiffTime, UTCTime, getCurrentTime, diffUTCTime)
import           Geometry.Circle
import           GHC.Stack
import           GL.Array
import           GL.Framebuffer
import           GL.Program
import           GL.Shader.DSL (defaultVars)
import           GL.Viewport
import           Graphics.GL.Core41
import           Lens.Micro (Lens', (.~), (^.), each, lens)
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
import           Starlight.Body as S
import           Starlight.Input
import qualified Starlight.Shader.Body as Body
import qualified Starlight.Shader.Radar as Radar
import qualified Starlight.Shader.Ship as Ship
import qualified Starlight.Shader.Stars as Stars
import qualified Starlight.Sol as S
import           System.FilePath
import qualified UI.Carrier.Window as Window
import           UI.Colour
import           UI.Label as Label
import           UI.Typeface (cacheCharactersForDrawing, readTypeface)
import           Unit.Angle
import           Unit.Length
import           Unit.Mass
import           Unit.Time

main :: HasCallStack => IO ()
main = E.handle (putStrLn . E.displayException @E.SomeException) $ reportTimings . fst <=< runProfile $ do
  system <- S.system

  Window.runWindow "Starlight" (V2 1024 768) . runFinally $ now >>= \ start ->
    evalState @Input mempty
    . evalState GameState
      { throttle = 20
      , player   = Actor
        { position = P (V2 25000 0)
        , velocity = V2 0 75
        , rotation = axisAngle (unit _z) (pi/2)
        , target   = Nothing
        }
      , npcs =
        [ Actor
          { position = P (V2 25000 0)
          , velocity = V2 0 75
          , rotation = axisAngle (unit _z) (pi/2)
          , target   = Nothing
          }
        ]
      , system = system
      }
    . evalState start $ do
      starsP <- build Stars.shader
      shipP  <- build Ship.shader
      radarP <- build Radar.shader
      bodyP  <- build Body.shader

      face <- measure "readTypeface" $ readTypeface ("fonts" </> "DejaVuSans.ttf")
      measure "cacheCharactersForDrawing" . cacheCharactersForDrawing face $ ['0'..'9'] <> ['a'..'z'] <> ['A'..'Z'] <> "./" -- characters to preload

      fpsL    <- measure "label" $ Label.label face
      targetL <- measure "label" $ Label.label face

      quadA   <- load quadV
      shipA   <- load shipV
      circleA <- load circleV
      radarA  <- load radarV

      let view = View{ quadA, shipA, circleA, radarA, starsP, shipP, radarP, bodyP, fpsL, targetL }

      glEnable GL_BLEND
      glEnable GL_DEPTH_CLAMP
      glEnable GL_SCISSOR_TEST
      glEnable GL_PROGRAM_POINT_SIZE

      put =<< now

      fix $ \ loop -> do
        continue <- measure "frame" $ do
          t <- realToFrac <$> since start
          system <- Lens.use _system
          let bodies = S.bodiesAt system (getDelta t)
          continue <- fmap isJust . runEmpty $ do
            input <- measure "input" input
            dt <- controls bodies fpsL targetL input
            gameState <- measure "physics" (physics bodies dt)
            draw view bodies gameState
          continue <$ measure "swap" Window.swap
        when continue loop

-- FIXME: organize timings into a tree
reportTimings :: Has (Lift IO) sig m => Timings -> m ()
reportTimings (Timings ts) = for_ (sortOn (Down . mean . snd) (Map.toList ts)) $ \ (l:|ls, t) -> sendM $ do
  putStrLn $ foldl' label (unpack l) ls <> ": " <> showTiming t where
  label l' l = unpack l <> "." <> l'
  showTiming t = "{min: " <> showMS (min' t) <> ", mean: " <> showMS (mean t) <> ", max: " <> showMS (max' t) <> "}"
  showMS = (<> "ms") . show . getSeconds . getMilli . milli @Seconds @Double . realToFrac


shipV :: [Ship.V I]
shipV = coerce @[V2 Float]
  [ V2 1      0
  , V2 0      (-0.5)
  , V2 (-0.5) 0
  , V2 0      0.5
  ]

quadV :: [Stars.V I]
quadV = coerce @[V2 Float]
  [ V2 (-1) (-1)
  , V2   1  (-1)
  , V2 (-1)   1
  , V2   1    1
  ]

circleV :: [Body.V I]
circleV = coerce @[V4 Float] . map (`ext` V2 0 1) $ circle 1 128

radarV :: [Radar.V I]
radarV = coerce @[Float] [ fromIntegral t / fromIntegral n | t <- [-n..n] ] where
  n = (16 :: Int)

controls
  :: ( Has (Lift IO) sig m
     , Has Profile sig m
     , Has (State Input) sig m
     , Has (State GameState) sig m
     , Has (State UTCTime) sig m
     )
  => [S.Instant Float]
  -> Label
  -> Label
  -> Input
  -> m (Delta Seconds Float)
controls bodies fpsL targetL input = measure "controls" $ do
  Delta (Seconds dt) <- fmap realToFrac . since =<< get
  put =<< now

  when (input ^. (_pressed SDL.KeycodePlus `or` _pressed SDL.KeycodeEquals)) $
    _throttle += dt * 10
  when (input ^. _pressed SDL.KeycodeMinus) $
    _throttle -= dt * 10

  thrust <- (dt *) <$> Lens.use _throttle

  let angular = dt *^ Radians 5

  when (input ^. _pressed SDL.KeycodeUp) $ do
    rotation <- Lens.use (_player . _rotation)
    _player . _velocity += rotate rotation (unit _x ^* thrust) ^. _xy
  when (input ^. _pressed SDL.KeycodeDown) $ do
    rotation <- Lens.use (_player . _rotation)
    velocity <- Lens.use (_player . _velocity)
    let angle = fst (polar2 (negated velocity))
        proposed = axisAngle (unit _z) (getRadians angle)
        delta = abs (wrap (Interval (-pi) pi) (snd (toAxisAngle rotation) - angle))
    _player . _rotation .= slerp rotation proposed (min 1 (getRadians (angular / delta)))

  when (input ^. _pressed SDL.KeycodeLeft) $
    _player . _rotation *= axisAngle (unit _z) (getRadians angular)
  when (input ^. _pressed SDL.KeycodeRight) $
    _player . _rotation *= axisAngle (unit _z) (getRadians (-angular))

  when (input ^. _pressed SDL.KeycodeTab) $ do
    shift <- Lens.use (_pressed SDL.KeycodeLShift `or` _pressed SDL.KeycodeRShift)
    _player . _target %= switchTarget shift
    _pressed SDL.KeycodeTab .= False

  name <- Lens.uses (_player . _target) (maybe "" (name . body . (bodies !!)))

  measure "setLabel" $ setLabel fpsL    18 (show (round (dt * 1000) :: Int) <> "ms/" <> show (roundToPlaces 1 (1/dt)) <> "fps")
  measure "setLabel" $ setLabel targetL 18 name

  pure (Delta (Seconds dt)) where
  switchTarget = \case
    False -> maybe (Just 0)                      (\ i -> i + 1 <$ guard (i + 1 < length bodies))
    True  -> maybe (Just (pred (length bodies))) (\ i -> i - 1 <$ guard (i - 1 >= 0))
  or = liftA2 (liftA2 (coerce (||)))


physics
  :: Has (State GameState) sig m
  => [S.Instant Float]
  -> Delta Seconds Float
  -> m GameState
physics bodies (Delta (Seconds dt)) = do
  scale <- Lens.uses _system scale
  _actors . each %= updatePosition . flip (foldr (applyGravity (1/scale))) bodies
  get where
  updatePosition a@Actor{ position, velocity } = a { position = position .+^ velocity }
  applyGravity distanceScale S.Instant{ transform, body = S.Body{ mass } } a@Actor{ position, velocity }
    = a { velocity = velocity + dt * force *^ normalize (pos ^-^ unP position) } where
    force = bigG * getKilograms mass / r -- assume actors’ mass is negligible
    pos = (transform !* V4 0 0 0 1) ^. _xy -- compute body location in 3d, but only use xy
    r = qd (pos ^* distanceScale) (unP position ^* distanceScale) -- “quadrance” (square of distance between actor & body)
    bigG = 6.67430e-11 -- gravitational constant


-- | Compute the zoom factor for the given velocity.
--
-- Higher values correlate to more of the scene being visible.
zoomForSpeed :: V2 Int -> Float -> Float
zoomForSpeed size x
  | I x < min_ speed = getI (min_ zoom)
  | I x > max_ speed = getI (max_ zoom)
  | otherwise        = getI (fromUnit zoom (coerce easeInOutCubic (toUnit speed (I x)))) where
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
  => View
  -> [S.Instant Float]
  -> GameState
  -> m ()
draw View{ quadA, circleA, shipA, radarA, shipP, starsP, radarP, bodyP, fpsL, targetL } bodies game = measure "draw" . runLiftIO $ do
  let Actor{ position, velocity, target } = game ^. _player
  bind @Framebuffer Nothing

  scale <- Window.scale
  size <- Window.size
  viewport $ scale *^ Interval 0 size
  scissor  $ scale *^ Interval 0 size

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
  let V2 sx sy = 1 / size ^* scale ^* (1 / zoomOut)
      origin
        =   scaled (V4 sx sy 1 1) -- transform the [[-1,1], [-1,1]] interval to window coordinates
        !*! translated3 (ext (negated (unP position)) 0) -- transform to the origin

  measure "ship" . use shipP $ do
    for_ (game ^. _actors) $ \ Actor{ position, rotation } -> do
      set Ship.U
        { matrix = Just
            $   origin
            !*! translated3 (ext (unP position) 0)
            !*! scaled (V4 15 15 15 1)
            !*! mkTransformation rotation 0
        , colour = Just white
        }

      bindArray shipA $
        drawArrays LineLoop (Interval 0 4)

  let onScreen S.Instant{ body = S.Body{ radius }, transform } = distance ((transform !* V4 0 0 0 1) ^. _xy) (unP position) - getMetres (game ^. _system . _scale *^ radius) < maximum (size ^* scale ^* zoomOut) * 0.5
      drawBody i@S.Instant{ body = S.Body{ radius = Metres r, colour }, transform, rotation } = when (onScreen i) $ do
        set Body.U
          { matrix = Just
              $   origin
              !*! transform
              !*! scaled (V4 r r r 1)
              !*! mkTransformation rotation 0
          , colour = Just colour
          }

        drawArraysInstanced LineLoop (Interval 0 (I (length circleV))) 3

  measure "bodies" $
    use bodyP . bindArray circleA $ origin `seq` for_ bodies drawBody

  measure "radar" . use radarP $ do
    set defaultVars
      { Radar.matrix = Just (scaled (V3 sx sy 1))
      }
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
                let edge = game ^. _system . _scale * r * (min d radius/d) *^ perp direction + direction ^* radius + here
                    sweep = max minSweep (abs (wrap (Interval (-pi) pi) (angleTo here edge - angle)))

                set Radar.U
                  { matrix = Nothing
                  , radius = Just radius
                  , angle  = Just angle
                  , sweep  = Just sweep
                  , colour = Just colour
                  }

                drawArrays LineStrip (Interval 0 (I (length radarV)))

          drawAtRadius 100 minSweep (colour & _a .~ 0.5)

          when (Just name == (target >>= \ i -> S.name (S.body (bodies !! i)) <$ guard (i < length bodies))) $ for_ [1..n] $ \ i ->
            drawAtRadius (step * fromIntegral i) (minSweep * Radians (fromIntegral i / 7)) ((colour + 0.5 * fromIntegral i / fromIntegral n) ** 2 & _a .~ (fromIntegral i / fromIntegral n))

    bindArray radarA $ for_ bodies drawBlip

  fpsSize <- labelSize fpsL
  measure "drawLabel" $ drawLabel fpsL    (V2 10 (floor (size ^. _y) - fpsSize ^. _y - 10)) white Nothing
  measure "drawLabel" $ drawLabel targetL (V2 10 10) white Nothing

roundToPlaces :: RealFloat a => Int -> a -> a
roundToPlaces n x = fromInteger (round (x * n')) / n' where
  n' = 10 ^ n


data View = View
  { quadA   :: Array (Stars.V I)
  , circleA :: Array (Body.V  I)
  , shipA   :: Array (Ship.V  I)
  , radarA  :: Array (Radar.V I)
  , starsP  :: Program Stars.U Stars.V Stars.O
  , shipP   :: Program Ship.U  Ship.V  Ship.O
  , bodyP   :: Program Body.U  Body.V  Body.O
  , radarP  :: Program Radar.U Radar.V Radar.O
  , fpsL    :: Label
  , targetL :: Label
  }


data GameState = GameState
  { throttle :: !Float
  , player   :: !Actor
  , npcs     :: ![Actor]
  , system   :: !(System Float)
  }
  deriving (Show)

_throttle :: Lens' GameState Float
_throttle = lens throttle (\ s v -> s { throttle = v })

_actors :: Lens' GameState (NonEmpty Actor)
_actors = lens ((:|) . player <*> npcs) (\ s (p:|o) -> s { player = p, npcs = o })

_player :: Lens' GameState Actor
_player = lens player (\ s p -> s { player = p })

_system :: Lens' GameState (System Float)
_system = lens system (\ s p -> s { system = p })


data Actor = Actor
  { position :: !(Point V2 Float)
  , velocity :: !(V2 Float)
  , rotation :: !(Quaternion Float)
  , target   :: !(Maybe Int)
  }
  deriving (Show)

_position :: Lens' Actor (Point V2 Float)
_position = lens position (\ s v -> s { position = v })

_velocity :: Lens' Actor (V2 Float)
_velocity = lens velocity (\ s v -> s { velocity = v })

_rotation :: Lens' Actor (Quaternion Float)
_rotation = lens Main.rotation (\ s r -> s { Main.rotation = r })

_target :: Lens' Actor (Maybe Int)
_target = lens target (\ s t -> s { target = t })



now :: Has (Lift IO) sig m => m UTCTime
now = sendM getCurrentTime

since :: Has (Lift IO) sig m => UTCTime -> m NominalDiffTime
since t = flip diffUTCTime t <$> now
