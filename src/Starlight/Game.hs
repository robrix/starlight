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
import           Control.Monad (when, (<=<))
import           Control.Monad.IO.Class.Lift (MonadIO, runLiftIO)
import           Data.Coerce
import           Data.Foldable (find, for_)
import           Data.Function (fix)
import           Data.Functor.Const
import           Data.Functor.I
import           Data.Functor.Interval
import           Data.Ix (inRange)
import           Data.List (elemIndex)
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Map as Map
import           Data.Maybe (isJust)
import           Data.Time.Clock (NominalDiffTime, UTCTime, diffUTCTime, getCurrentTime)
import           GL.Framebuffer
import           GL.Viewport
import           Graphics.GL.Core41
import           Lens.Micro (Lens', each, lens, (^.))
import           Linear.Affine
import           Linear.Exts
import           Linear.Metric
import           Linear.Quaternion
import           Linear.V2 as Linear
import           Linear.V3 as Linear
import           Linear.Vector as Linear
import           Numeric
import qualified SDL
import           Starlight.Actor as Actor
import           Starlight.AI
import           Starlight.Body as Body
import           Starlight.CLI
import           Starlight.Identifier
import           Starlight.Input
import           Starlight.Physics
import           Starlight.Radar as Radar
import           Starlight.Ship as Ship
import qualified Starlight.Sol as Sol
import           Starlight.Starfield as Starfield
import           Starlight.System
import           Starlight.View
import           Starlight.Weapon.Laser as Laser
import           System.Environment
import           System.Exit
import           System.FilePath
import           UI.Colour
import           UI.Label as Label
import           UI.Typeface (Font(Font), cacheCharactersForDrawing, readTypeface)
import qualified UI.Window as Window
import           Unit.Angle
import           Unit.Length
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
  system <- Sol.system

  Window.runWindow "Starlight" (V2 1024 768)
    . runFinally
    . evalState @Input mempty
    . evalState GameState
      { throttle = 20
      , player   = Actor
        { position = P (V2 250000 0)
        , velocity = V2 0 150
        , rotation = axisAngle (unit _z) (pi/2)
        , target   = Nothing
        , health   = 1000
        }
      , firing   = False
      , npcs     =
        [ Actor
          { position = P (V2 250000 0)
          , velocity = V2 0 150
          , rotation = axisAngle (unit _z) (pi/2)
          , target   = Just $ Star (10, "Sol")
          , health   = 100
          }
        , Actor
          { position = P (V2 250000 0)
          , velocity = V2 0 150
          , rotation = axisAngle (unit _z) (pi/2)
          , target   = Just $ Star (10, "Sol") :/ (199, "Mercury")
          , health   = 100
          }
        ]
      , system
      } $ do
      face <- measure "readTypeface" $ readTypeface ("fonts" </> "DejaVuSans.ttf")
      measure "cacheCharactersForDrawing" . cacheCharactersForDrawing face $ ['0'..'9'] <> ['a'..'z'] <> ['A'..'Z'] <> "./:-" -- characters to preload

      fpsL    <- measure "label" Label.label
      targetL <- measure "label" Label.label

      glEnable GL_BLEND
      glEnable GL_DEPTH_CLAMP
      glEnable GL_SCISSOR_TEST
      glEnable GL_PROGRAM_POINT_SIZE

      start <- now
      evalState start . runStarfield . runShip . runRadar . runLaser . runBody . fix $ \ loop -> do
        continue <- measure "frame" $ do
          t <- realToFrac <$> since start
          system <- Lens.use _system
          continue <- evalEmpty . runReader (systemAt system (getDelta t)) $ do
            measure "input" input
            dt <- fmap realToFrac . since =<< get
            put =<< now
            controls dt
            system <- ask
            measure "ai"      (_npcs   . each %= ai      dt system)
            measure "physics" (_actors . each %= physics dt system)
            gameState <- get
            withView gameState (draw dt fpsL targetL (Font face 18) gameState)
          continue <$ measure "swap" Window.swap
        when continue loop

controls
  :: ( Has (Lift IO) sig m
     , Has Profile sig m
     , Has (Reader (System StateVectors Float)) sig m
     , Has (State Input) sig m
     , Has (State GameState) sig m
     )
  => Delta Seconds Float
  -> m ()
controls (Delta (Seconds dt)) = measure "controls" $ do
  input <- get
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

  _firing .= input ^. _pressed SDL.KeycodeSpace

  System{ bodies } <- ask @(System StateVectors Float)
  let identifiers = Map.keys bodies
      switchTarget shift target = case target >>= (`elemIndex` identifiers) of
        Just i  -> identifiers !! i' <$ guard (inRange (0, pred (length bodies)) i') where
          i' | shift     = i - 1
             | otherwise = i + 1
        Nothing -> Just $ if shift then last identifiers else head identifiers
  when (input ^. _pressed SDL.KeycodeTab) $ do
    shift <- Lens.use (_pressed SDL.KeycodeLShift `or` _pressed SDL.KeycodeRShift)
    _player . _target %= switchTarget shift
    _pressed SDL.KeycodeTab .= False
  where
  or = liftA2 (liftA2 (coerce (||)))

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

draw
  :: ( Has Finally sig m
     , Has (Lift IO) sig m
     , Has Profile sig m
     , Has (Reader Body.Drawable) sig m
     , Has (Reader Laser.Drawable) sig m
     , Has (Reader Radar.Drawable) sig m
     , Has (Reader Ship.Drawable) sig m
     , Has (Reader Starfield.Drawable) sig m
     , Has (Reader (System StateVectors Float)) sig m
     , Has (Reader View) sig m
     )
  => Delta Seconds Float
  -> Label
  -> Label
  -> Font
  -> GameState
  -> m ()
draw dt fpsL targetL font game = measure "draw" . runLiftIO $ do
  let Actor{ position, rotation, target } = game ^. _player
  bind @Framebuffer Nothing

  View{ scale, size, zoom } <- ask

  viewport $ scale *^ Interval 0 size
  scissor  $ scale *^ Interval 0 size

  glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA

  drawStarfield

  for_ (game ^. _actors) (drawShip white)

  when (game ^. _firing) $ drawLaser green (snd (toAxisAngle rotation))

  let maxDim = maximum (fromIntegral <$> size ^* scale) * zoom

  System{ scale, bodies } <- ask @(System StateVectors Float)

  let onScreen StateVectors{ body = Body{ radius }, position = pos } = distance pos position - scale * getMetres radius < maxDim * 0.5

  for_ bodies $ \ sv -> when (onScreen sv) (drawBody sv)

  drawRadar (player game) (npcs game)

  let describeTarget target = case target >>= \ i -> find ((== i) . identifier . Body.body) bodies of
        Just StateVectors{ body, position = pos } -> describeIdentifier (identifier body) ++ ": " ++ showEFloat (Just 1) (kilo (Metres (distance (pos ^* (1/scale)) (position ^* (1/scale))))) "km"
        _ -> ""

  measure "setLabel" $ setLabel fpsL    font (showFFloat (Just 1) (dt * 1000) "ms/" <> showFFloat (Just 1) (1/dt) "fps")
  measure "setLabel" $ setLabel targetL font (describeTarget target)

  fpsSize <- labelSize fpsL
  measure "drawLabel" $ drawLabel fpsL    (V2 10 (size ^. _y - fpsSize ^. _y - 10)) white Nothing
  measure "drawLabel" $ drawLabel targetL (V2 10 10) white Nothing

withView
  :: ( Has (Lift IO) sig m
     , Has (Reader Window.Window) sig m
     )
  => GameState
  -> ReaderC View m a
  -> m a
withView game m = do
  scale <- Window.scale
  size  <- Window.size
  let velocity = game ^. _player . _velocity
      zoom = zoomForSpeed size (norm velocity)
      focus = game ^. _player . _position
  runReader View{ scale, size, zoom, focus } m


data GameState = GameState
  { throttle :: !Float
  , player   :: !Actor
  , firing   :: !Bool
  , npcs     :: ![Actor]
  , system   :: !(System Body Float)
  }
  deriving (Show)

_throttle :: Lens' GameState Float
_throttle = lens throttle (\ s v -> s { throttle = v })

_player :: Lens' GameState Actor
_player = lens player (\ s p -> s { player = p })

_firing :: Lens' GameState Bool
_firing = lens firing (\ s p -> s { firing = p })

_npcs :: Lens' GameState [Actor]
_npcs = lens npcs (\ s n -> s { npcs = n })

_actors :: Lens' GameState (NonEmpty Actor)
_actors = lens ((:|) . player <*> npcs) (\ s (p:|o) -> s { player = p, npcs = o })

_system :: Lens' GameState (System Body Float)
_system = lens system (\ s p -> s { system = p })


now :: Has (Lift IO) sig m => m UTCTime
now = sendM getCurrentTime

since :: Has (Lift IO) sig m => UTCTime -> m NominalDiffTime
since t = flip diffUTCTime t <$> now


evalEmpty :: Functor m => EmptyC m a -> m Bool
evalEmpty = fmap isJust . runEmpty
