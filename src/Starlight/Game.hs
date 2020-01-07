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
( game
) where

import           Control.Algebra
import           Control.Carrier.Empty.Maybe
import           Control.Carrier.Fail.Either
import           Control.Carrier.Finally
import           Control.Carrier.Reader
import           Control.Carrier.State.Strict
import           Control.Effect.Lens.Exts as Lens
import           Control.Effect.Profile
import           Control.Effect.Trace
import           Control.Lens (itraverse, to, (%~), (^.))
import           Control.Monad (when, (>=>))
import           Control.Monad.IO.Class.Lift
import           Data.Coerce
import           Data.Function (fix)
import           Data.Functor.Identity
import           Data.Functor.Interval
import qualified Data.Map as Map
import           Data.Maybe (isJust)
import           Data.Time.Clock (NominalDiffTime, UTCTime, diffUTCTime, getCurrentTime)
import           Data.Time.Format.ISO8601
import           GL
import           GL.Effect.Check
import           Linear.Exts
import           Starlight.Actor
import           Starlight.AI
import           Starlight.Body
import           Starlight.Character
import           Starlight.Controls
import           Starlight.Draw
import           Starlight.Draw.Body as Body
import           Starlight.Draw.Radar as Radar
import           Starlight.Draw.Ship as Ship
import           Starlight.Draw.Starfield as Starfield
import           Starlight.Draw.Weapon.Laser as Laser
import           Starlight.Identifier
import           Starlight.Input
import           Starlight.Physics
import           Starlight.Ship
import qualified Starlight.Sol as Sol
import           Starlight.System as System
import           Starlight.UI
import           Starlight.View
import           System.FilePath
import           UI.Colour
import           UI.Context
import           UI.Label as Label
import           UI.Typeface (cacheCharactersForDrawing, readTypeface)
import qualified UI.Window as Window
import           Unit.Length
import           Unit.Time

runGame
  :: Has (Lift IO) sig m
  => System Body
  -> StateC (System Body) (StateC Input (FinallyC (GLC (ReaderC Context (ReaderC Window.Window m))))) a
  -> m a
runGame system
  = Window.runSDL
  . Window.runWindow "Starlight" (V2 1024 768)
  . runContext
  . runGLC
  . runFinally
  . evalState @Input mempty
  . evalState system
      { characters = Map.fromList $ zip (Player : map NPC [0..])
        [ Character
          { actor   = Actor
            { position = P (V3 2500000 0 0)
            , velocity = V3 0 150 0
            , rotation = axisAngle (unit _z) (pi/2)
            }
          , target  = Nothing
          , actions = mempty
          , ship    = Ship{ colour = white, armour = 1000, scale = 15 }
          }
        , Character
          { actor   = Actor
            { position = P (V3 2500000 0 0)
            , velocity = V3 0 150 0
            , rotation = axisAngle (unit _z) (pi/2)
            }
          , target  = Just (C Player)
          , actions = mempty
          , ship    = Ship{ colour = red, armour = Interval 500 500, scale = 30 }
          }
        , Character
          { actor   = Actor
            { position = P (V3 2500000 0 0)
            , velocity = V3 0 150 0
            , rotation = axisAngle (unit _z) (pi/2)
            }
          , target  = Just $ B (Star (10, "Sol"))
          , actions = mempty
          , ship    = Ship{ colour = white, armour = 1000, scale = 15 }
          }
        , Character
          { actor   = Actor
            { position = P (V3 2500000 0 0)
            , velocity = V3 0 150 0
            , rotation = axisAngle (unit _z) (pi/2)
            }
          , target  = Just $ B (Star (10, "Sol") :/ (199, "Mercury"))
          , actions = mempty
          , ship    = Ship{ colour = white, armour = 1000, scale = 15 }
          }
        ]
      }

runFrame
  :: ( Effect sig
     , Has Check sig m
     , Has Finally sig m
     , Has (Lift IO) sig m
     , Has Trace sig m
     )
  => ReaderC Body.Drawable (ReaderC Laser.Drawable (ReaderC Radar.Drawable (ReaderC Ship.Drawable (ReaderC Starfield.Drawable (StateC UTCTime m))))) a
  -> m a
runFrame m = do
  start <- now
  evalState start . runStarfield . runShip . runRadar . runLaser . runBody $ m

game
  :: ( Effect sig
     , Has Check sig m
     , Has (Lift IO) sig m
     , Has Profile sig m
     , Has Trace sig m
     )
  => m ()
game = Sol.system >>= \ system -> runGame system $ do
  trace "loading typeface"
  face <- measure "readTypeface" $ readTypeface ("fonts" </> "DejaVuSans.ttf")
  measure "cacheCharactersForDrawing" . cacheCharactersForDrawing face $ ['0'..'9'] <> ['a'..'z'] <> ['A'..'Z'] <> "./:-" -- characters to preload

  fpsLabel    <- measure "label" Label.label
  targetLabel <- measure "label" Label.label

  enabled_ Blend            .= True
  enabled_ DepthClamp       .= True
  enabled_ LineSmooth       .= True
  enabled_ ProgramPointSize .= True
  enabled_ ScissorTest      .= True

  -- J2000
  epoch <- either (pure . error) pure =<< runFail (iso8601ParseM "2000-01-01T12:00:00.000Z")

  runFrame . runReader UI{ fps = fpsLabel, target = targetLabel, face } . fix $ \ loop -> do
    continue <- measure "frame" (runSystem epoch (timed (execEmpty frame)))
    when continue (measure "swap" Window.swap *> loop)

frame
  :: ( Effect sig
     , Has Check sig m
     , Has Empty sig m
     , Has (Lift IO) sig m
     , Has Profile sig m
     , Has (Reader (Delta Seconds Float)) sig m
     , Has (Reader Body.Drawable) sig m
     , Has (Reader Laser.Drawable) sig m
     , Has (Reader Radar.Drawable) sig m
     , Has (Reader Ship.Drawable) sig m
     , Has (Reader Starfield.Drawable) sig m
     , Has (Reader (System StateVectors)) sig m
     , Has (Reader UI) sig m
     , Has (Reader Window.Window) sig m
     , Has (State Input) sig m
     , Has (State (System Body)) sig m
     )
  => m ()
frame = do
  withView draw -- draw with current readonly positions & beams
  measure "inertia" $ characters_ @Body %= fmap (actor_%~inertia) -- update positions

  measure "input" input
  measure "controls" $ player_ @Body .actions_ <~ controls
  measure "ai" $ npcs_ @Body <~> traverse ai

  -- FIXME: this is so gross
  beams_ @Body .= []
  npcs_ @Body %= filter (\ Character{ ship = Ship{ armour } } -> armour^.min_ > 0)
  characters_ @Body <~> itraverse
    (\ i
    ->  measure "gravity" . (actor_ @Character <-> gravity)
    >=> measure "hit" . hit i
    >=> measure "runActions" . runActions i)

-- | Compute the zoom factor for the given velocity.
--
-- Higher values correlate to more of the scene being visible.
zoomForSpeed :: V2 Int -> Float -> Float
zoomForSpeed size x = go where
  Identity go
    | Identity x < min' speed = min' zoom
    | Identity x > max' speed = max' zoom
    | otherwise               = fromUnit zoom (coerce easeInOutCubic (toUnit speed (Identity x)))
  zoom = Interval 1 5
  speed = speedAt <$> zoom
  speedAt x = x / 25 * fromIntegral (maximum size)

withView
  :: ( Has (Lift IO) sig m
     , Has (Reader (System StateVectors)) sig m
     , Has (Reader Window.Window) sig m
     )
  => ReaderC View m a
  -> m a
withView m = do
  scale <- Window.scale
  size  <- Window.size

  velocity <- view (player_ @StateVectors .actor_.velocity_)
  focus    <- view (player_ @StateVectors .actor_.position_._xy.to P)

  let zoom = zoomForSpeed size (norm velocity)
  runReader View{ scale, size, zoom, focus } m


now :: Has (Lift IO) sig m => m UTCTime
now = sendM getCurrentTime

since :: Has (Lift IO) sig m => UTCTime -> m NominalDiffTime
since t = flip diffUTCTime t <$> now

timed
  :: ( Has (Lift IO) sig m
     , Has (State UTCTime) sig m
     )
  => ReaderC (Delta Seconds Float) m a
  -> m a
timed m = do
  dt <- fmap realToFrac . since =<< get
  put =<< now
  runReader dt m


newtype Epoch = Epoch { getEpoch :: UTCTime }
  deriving (ISO8601)

-- | Run an action in an ephemeral system derived from the persistent system.
runSystem
  :: ( Has (Lift IO) sig m
     , Has (State (System Body)) sig m
     )
  => Epoch
  -> ReaderC (System StateVectors) m a
  -> m a
runSystem epoch m = do
  t <- realToFrac <$> since (getEpoch epoch)
  system <- get
  runReader (systemAt system (getDelta t)) m


execEmpty :: Functor m => EmptyC m a -> m Bool
execEmpty = fmap isJust . runEmpty
