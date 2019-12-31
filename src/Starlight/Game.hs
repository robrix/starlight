{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module Starlight.Game
( main
) where

import           Control.Applicative (liftA2)
import           Control.Carrier.Empty.Maybe
import           Control.Carrier.Finally
import qualified Control.Carrier.Profile.Identity as NoProfile
import qualified Control.Carrier.Profile.Time as Profile
import           Control.Carrier.Reader
import           Control.Carrier.State.Strict
import           Control.Effect.Lens ((%=), (*=), (+=), (-=), (.=))
import qualified Control.Effect.Lens as Lens
import           Control.Effect.Lift
import           Control.Effect.Profile
import qualified Control.Exception.Lift as E
import           Control.Monad ((<=<), when)
import           Control.Monad.IO.Class.Lift (MonadIO, runLiftIO)
import           Data.Coerce
import           Data.Foldable (for_)
import           Data.Function (fix)
import           Data.Functor.Const
import           Data.Functor.I
import           Data.Functor.Interval
import           Data.List.NonEmpty (NonEmpty(..))
import           Data.Maybe (isJust)
import           Data.Time.Clock (NominalDiffTime, UTCTime, getCurrentTime, diffUTCTime)
import           Geometry.Circle
import           GL.Array
import           GL.Framebuffer
import           GL.Program
import           GL.Viewport
import           Graphics.GL.Core41
import           Lens.Micro (Lens', (^.), each, lens)
import           Linear.Affine
import           Linear.Exts
import           Linear.Matrix
import           Linear.Metric
import           Linear.Quaternion
import           Linear.V2 as Linear
import           Linear.V3 as Linear
import           Linear.V4
import           Linear.Vector as Linear
import           Numeric
import           Physics.Delta
import qualified SDL
import           Starlight.Actor as Actor
import           Starlight.Body as S
import           Starlight.CLI
import           Starlight.Input
import           Starlight.Radar as Radar
import qualified Starlight.Shader.Body as Body
import qualified Starlight.Shader.Ship as Ship
import qualified Starlight.Shader.Stars as Stars
import qualified Starlight.Sol as S
import           Starlight.View
import           System.Environment
import           System.Exit
import           System.FilePath
import           UI.Colour
import           UI.Label as Label
import           UI.Typeface (Font(Font), cacheCharactersForDrawing, readTypeface)
import qualified UI.Window as Window
import           Unit.Angle
import           Unit.Length
import           Unit.Mass
import           Unit.Time

main :: IO ()
main = handling $ do
  Options{ profile } <- execParser argumentsParser
  if profile then
    Profile.reportTimings . fst <=< Profile.runProfile $ runGame
  else
    NoProfile.runProfile runGame
  where
  handling m = do
    name <- getProgName
    -- Exceptions don’t seem to exit in the repl for unknown reasons, so we catch and log them (except for 'ExitCode')
    if name == "<interactive>" then
      m `E.catches`
        [ E.Handler (const @_ @ExitCode (pure ()))
        , E.Handler (putStrLn . E.displayException @E.SomeException)
        ]
    else
      m

runGame
  :: ( Effect sig
     , Has (Lift IO) sig m
     , Has Profile sig m
     , MonadFail m
     , MonadIO m
     )
  => m ()
runGame = do
  system <- S.system

  Window.runWindow "Starlight" (V2 1024 768) . runFinally $ now >>= \ start ->
    evalState @Input mempty
    . evalState GameState
      { throttle = 20
      , player   = Actor
        { position = P (V2 250000 0)
        , velocity = V2 0 150
        , rotation = axisAngle (unit _z) (pi/2)
        , target   = Nothing
        }
      , npcs =
        [ Actor
          { position = P (V2 250000 0)
          , velocity = V2 0 150
          , rotation = axisAngle (unit _z) (pi/2)
          , target   = Nothing
          }
        ]
      , system = system
      }
    . evalState start $ do
      starsP <- build Stars.shader
      shipP  <- build Ship.shader
      bodyP  <- build Body.shader

      face <- measure "readTypeface" $ readTypeface ("fonts" </> "DejaVuSans.ttf")
      measure "cacheCharactersForDrawing" . cacheCharactersForDrawing face $ ['0'..'9'] <> ['a'..'z'] <> ['A'..'Z'] <> "./:" -- characters to preload

      fpsL    <- measure "label" Label.label
      targetL <- measure "label" Label.label

      quadA   <- load quadV
      shipA   <- load shipV
      circleA <- load circleV

      radar <- Radar.radar

      let view = View{ quadA, shipA, circleA, radar, starsP, shipP, bodyP, fpsL, targetL, font = Font face 18 }

      glEnable GL_BLEND
      glEnable GL_DEPTH_CLAMP
      glEnable GL_SCISSOR_TEST
      glEnable GL_PROGRAM_POINT_SIZE

      put =<< now

      fix $ \ loop -> do
        continue <- measure "frame" $ do
          t <- realToFrac <$> since start
          system <- Lens.use _system
          continue <- fmap isJust . runEmpty . runReader (S.bodiesAt system (getDelta t)) $ do
            input <- measure "input" input
            dt <- fmap realToFrac . since =<< get
            put =<< now
            controls view dt input
            ai dt
            gameState <- measure "physics" (physics dt)
            draw view gameState
          continue <$ measure "swap" Window.swap
        when continue loop


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

controls
  :: ( Has (Lift IO) sig m
     , Has Profile sig m
     , Has (Reader [S.StateVectors Float]) sig m
     , Has (State Input) sig m
     , Has (State GameState) sig m
     )
  => View
  -> Delta Seconds Float
  -> Input
  -> m ()
controls View{ fpsL, targetL, font } (Delta (Seconds dt)) input = measure "controls" $ do
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
    _player . _rotation .= face angular (angleOf (negated velocity)) rotation

  when (input ^. _pressed SDL.KeycodeLeft) $
    _player . _rotation *= axisAngle (unit _z) (getRadians angular)
  when (input ^. _pressed SDL.KeycodeRight) $
    _player . _rotation *= axisAngle (unit _z) (getRadians (-angular))

  bodies <- ask
  let switchTarget = \case
        False -> maybe (Just 0)                      (\ i -> i + 1 <$ guard (i + 1 < length bodies))
        True  -> maybe (Just (pred (length bodies))) (\ i -> i - 1 <$ guard (i - 1 >= 0))
  when (input ^. _pressed SDL.KeycodeTab) $ do
    shift <- Lens.use (_pressed SDL.KeycodeLShift `or` _pressed SDL.KeycodeRShift)
    _player . _target %= switchTarget shift
    _pressed SDL.KeycodeTab .= False

  position <- Lens.use (_player . _position)
  let describeTarget i
        | S.StateVectors{ scale, body, position = pos } <- bodies !! i
        = name body ++ ": " ++ showEFloat (Just 1) (kilo (Metres (distance (pos ^* (1/scale)) (unP position ^* scale)))) "km"
  target <- Lens.uses (_player . _target) (maybe "" describeTarget)

  measure "setLabel" $ setLabel fpsL    font (showFFloat (Just 1) (dt * 1000) "ms/" <> showFFloat (Just 1) (1/dt) "fps")
  measure "setLabel" $ setLabel targetL font target
  where
  or = liftA2 (liftA2 (coerce (||)))

-- | Compute a rotation turning to face a desired angle with a given maximum angular thrust.
face
  :: Radians Float    -- ^ Angular thrust. (Speed of rotation.)
  -> Radians Float    -- ^ Desired angle.
  -> Quaternion Float -- ^ Current rotation.
  -> Quaternion Float -- ^ Resulting rotation.
face angular angle rotation = slerp rotation proposed (min 1 (getRadians (angular / delta))) where
  proposed = axisAngle (unit _z) (getRadians angle)
  delta = abs (wrap (Interval (-pi) pi) (snd (toAxisAngle rotation) - angle))


ai
  :: ( Has (Reader [S.StateVectors Float]) sig m
     , Has (State GameState) sig m
     )
  => Delta Seconds Float
  -> m ()
ai (Delta (Seconds dt)) = do
  bodies <- ask
  _npcs . each %= go bodies
  where
  go bodies a@Actor{ target, velocity, rotation, position } = case target of
    -- FIXME: different kinds of behaviours: aggressive, patrolling, mining, trading, etc.
    Just i
      | S.StateVectors{ position = pos } <- bodies !! i
      , angle     <- angleTo (unP position) pos
      , rotation' <- face angular angle rotation
      -> a
        { Actor.rotation = rotation'
        -- FIXME: don’t just fly directly at the target, dumbass
        -- FIXME: factor in the target’s velocity & distance
        -- FIXME: allow other behaviours relating to targets, e.g. following
        , velocity = if abs (wrap (Interval (-pi) pi) (snd (toAxisAngle rotation') - angle)) < pi/2 then velocity + rotate rotation' (unit _x ^* thrust) ^. _xy else velocity
        }
    -- FIXME: wander
    -- FIXME: pick a new target
    Nothing -> a
  angular = dt *^ Radians 5
  thrust  = dt * 1


physics
  :: ( Has (Reader [S.StateVectors Float]) sig m
     , Has (State GameState) sig m
     )
  => Delta Seconds Float
  -> m GameState
physics (Delta (Seconds dt)) = do
  bodies <- ask @[S.StateVectors Float]
  scale <- Lens.uses (_system . _scale) (1/)
  _actors . each %= updatePosition . flip (foldr (applyGravity scale)) bodies
  get where
  updatePosition a@Actor{ position, velocity } = a { Actor.position = position .+^ velocity }
  applyGravity distanceScale S.StateVectors{ position = pos, body = S.Body{ mass } } a@Actor{ position, velocity }
    = a { velocity = velocity + dt * force *^ direction pos (unP position) } where
    force = bigG * getKilograms mass / r -- assume actors’ mass is negligible
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
     , Has (Reader [S.StateVectors Float]) sig m
     , Has (Reader Window.Window) sig m
     )
  => View
  -> GameState
  -> m ()
draw View{ quadA, circleA, shipA, radar, shipP, starsP, bodyP, fpsL, targetL } game = measure "draw" . runLiftIO . withViewScale $ do
  let Actor{ position } = game ^. _player
  bind @Framebuffer Nothing

  viewScale@ViewScale{ scale, size, zoom } <- ask

  viewport $ scale *^ Interval 0 size
  scissor  $ scale *^ Interval 0 size

  glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA

  measure "stars" . use starsP $ do
    set Stars.U
      { resolution = Just (fromIntegral <$> size ^* scale)
      , origin     = Just (position / P (fromIntegral <$> size))
      , zoom       = Just zoom
      }

    bindArray quadA $
      drawArrays TriangleStrip (Interval 0 4)

  let origin
        =   scaleToViewZoomed viewScale
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

  let maxDim = maximum ((fromIntegral <$> size ^* scale) ^* zoom)
      onScreen S.StateVectors{ scale, body = S.Body{ radius }, position = pos } = distance pos (unP position) - getMetres (scale *^ radius) < maxDim * 0.5
      drawBody i@S.StateVectors{ body = S.Body{ radius = Metres r, colour }, transform, rotation } = when (onScreen i) $ do
        set Body.U
          { matrix = Just
              $   origin
              !*! transform
              !*! scaled (V4 r r r 1)
              !*! mkTransformation rotation 0
          , colour = Just colour
          }

        drawArraysInstanced LineLoop (Interval 0 (I (length circleV))) 3

  bodies <- ask @[S.StateVectors Float]
  measure "bodies" $
    use bodyP . bindArray circleA $ origin `seq` for_ bodies drawBody

  drawRadar radar (player game) (npcs game)

  fpsSize <- labelSize fpsL
  measure "drawLabel" $ drawLabel fpsL    (V2 10 (size ^. _y - fpsSize ^. _y - 10)) white Nothing
  measure "drawLabel" $ drawLabel targetL (V2 10 10) white Nothing
  where
  withViewScale m = do
    scale <- Window.scale
    size  <- Window.size
    let velocity = game ^. _player . _velocity
        zoom = zoomForSpeed size (norm velocity)
    runReader ViewScale{ scale, size, zoom } m


data View = View
  { quadA   :: Array (Stars.V I)
  , circleA :: Array (Body.V  I)
  , shipA   :: Array (Ship.V  I)
  , starsP  :: Program Stars.U Stars.V Stars.O
  , shipP   :: Program Ship.U  Ship.V  Ship.O
  , bodyP   :: Program Body.U  Body.V  Body.O
  , radar   :: Radar
  , fpsL    :: Label
  , targetL :: Label
  , font    :: Font
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

_npcs :: Lens' GameState [Actor]
_npcs = lens npcs (\ s n -> s { npcs = n })

_actors :: Lens' GameState (NonEmpty Actor)
_actors = lens ((:|) . player <*> npcs) (\ s (p:|o) -> s { player = p, npcs = o })

_player :: Lens' GameState Actor
_player = lens player (\ s p -> s { player = p })

_system :: Lens' GameState (System Float)
_system = lens system (\ s p -> s { system = p })


now :: Has (Lift IO) sig m => m UTCTime
now = sendM getCurrentTime

since :: Has (Lift IO) sig m => UTCTime -> m NominalDiffTime
since t = flip diffUTCTime t <$> now
