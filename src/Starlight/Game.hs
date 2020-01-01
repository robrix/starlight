{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module Starlight.Game
( main
) where

import           Control.Carrier.Empty.Maybe
import           Control.Carrier.Finally
import qualified Control.Carrier.Profile.Identity as NoProfile
import qualified Control.Carrier.Profile.Time as Profile
import           Control.Carrier.Reader
import           Control.Carrier.State.Strict
import           Control.Effect.Lens ((%=), (.=))
import qualified Control.Effect.Lens as Lens
import           Control.Effect.Lift
import           Control.Effect.Profile
import qualified Control.Exception.Lift as E
import           Control.Monad (when, (<=<))
import           Control.Monad.IO.Class.Lift (MonadIO, runLiftIO)
import           Data.Coerce
import           Data.Foldable (find, for_)
import           Data.Function (fix)
import           Data.Functor.Identity
import           Data.Functor.Interval
import           Data.List.NonEmpty (NonEmpty(..))
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
import           Starlight.Actor as Actor
import           Starlight.AI
import           Starlight.Body as Body
import           Starlight.CLI
import           Starlight.Controls
import           Starlight.Identifier
import           Starlight.Input
import           Starlight.Physics
import           Starlight.Player
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
      { player = Player
        { actor    = Actor
          { position = P (V2 250000 0)
          , velocity = V2 0 150
          , rotation = axisAngle (unit _z) (pi/2)
          , target   = Nothing
          , health   = 1000
          }
        , throttle = 20
        , firing   = False
        }
      , npcs   =
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
            measure "controls" $ _player %%= controls dt
            system <- ask
            measure "ai"      (_npcs   . each %= ai      dt system)
            measure "physics" (_actors . each %= physics dt system)
            gameState <- get
            withView gameState (draw dt fpsL targetL (Font face 18) gameState)
          continue <$ measure "swap" Window.swap
        when continue loop

(%%=) :: Has (State s) sig m => Lens' s a -> StateC a m () -> m ()
lens %%= action = Lens.use lens >>= (`execState` action) >>= (lens .=)

infix 4 %%=

-- | Compute the zoom factor for the given velocity.
--
-- Higher values correlate to more of the scene being visible.
zoomForSpeed :: V2 Int -> Float -> Float
zoomForSpeed size x
  | Identity x < min_ speed = runIdentity (min_ zoom)
  | Identity x > max_ speed = runIdentity (max_ zoom)
  | otherwise        = runIdentity (fromUnit zoom (coerce easeInOutCubic (toUnit speed (Identity x)))) where
  zoom = Interval 1 6
  speed = speedAt <$> zoom
  speedAt x = x / 25 * fromIntegral (maximum size)

draw
  :: ( Has (Lift IO) sig m
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
  let Actor{ position, rotation, target } = game ^. _player . _actor
  bind @Framebuffer Nothing

  View{ scale, size, zoom } <- ask

  viewport $ scale *^ Interval 0 size
  scissor  $ scale *^ Interval 0 size

  glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA

  drawStarfield

  for_ (game ^. _actors) (drawShip white)

  when (game ^. _player . _firing) $ drawLaser green (snd (toAxisAngle rotation))

  let maxDim = maximum (fromIntegral <$> size ^* scale) * zoom

  System{ scale, bodies } <- ask @(System StateVectors Float)

  let onScreen StateVectors{ body = Body{ radius }, position = pos } = distance pos position - scale * getMetres radius < maxDim * 0.5

  for_ bodies $ \ sv -> when (onScreen sv) (drawBody sv)

  drawRadar (game ^. _player . _actor) (npcs game)

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
  let velocity = game ^. _player . _actor . _velocity
      zoom = zoomForSpeed size (norm velocity)
      focus = game ^. _player . _actor . _position
  runReader View{ scale, size, zoom, focus } m

data GameState = GameState
  { player   :: !Player
  , npcs     :: ![Actor]
  , system   :: !(System Body Float)
  }
  deriving (Show)

_player :: Lens' GameState Player
_player = lens player (\ s p -> s { player = p })

_npcs :: Lens' GameState [Actor]
_npcs = lens npcs (\ s n -> s { npcs = n })

_actors :: Lens' GameState (NonEmpty Actor)
_actors = lens ((:|) . actor . player <*> npcs) (\ s (a:|o) -> s { player = (player s) { actor = a }, npcs = o })

_system :: Lens' GameState (System Body Float)
_system = lens system (\ s p -> s { system = p })


now :: Has (Lift IO) sig m => m UTCTime
now = sendM getCurrentTime

since :: Has (Lift IO) sig m => UTCTime -> m NominalDiffTime
since t = flip diffUTCTime t <$> now


evalEmpty :: Functor m => EmptyC m a -> m Bool
evalEmpty = fmap isJust . runEmpty
