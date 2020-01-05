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
import           Control.Lens (to, (%~), (^.))
import           Control.Monad (when, (>=>))
import           Control.Monad.IO.Class.Lift
import           Data.Coerce
import           Data.Function (fix)
import           Data.Functor.Identity
import           Data.Functor.Interval
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
import           Starlight.Draw.Body
import           Starlight.Draw.Radar
import           Starlight.Draw.Ship
import           Starlight.Draw.Starfield
import           Starlight.Draw.Weapon.Laser
import           Starlight.Identifier
import           Starlight.Input
import           Starlight.Physics
import           Starlight.Ship
import qualified Starlight.Sol as Sol
import           Starlight.System as System
import           Starlight.View
import           System.FilePath
import           UI.Colour
import           UI.Label as Label
import           UI.Typeface (Font(Font), cacheCharactersForDrawing, readTypeface)
import qualified UI.Window as Window
import           Unit.Length

game
  :: ( Effect sig
     , Has Check sig m
     , Has (Lift IO) sig m
     , Has Profile sig m
     , Has Trace sig m
     )
  => m ()
game = do
  system <- Sol.system

  Window.runWindow "Starlight" (V2 1024 768)
    . runGLC
    . runFinally
    . evalState @Input mempty
    . evalState system
        { player = Character
          { actor   = Actor
            { position = P (V3 2500000 0 0)
            , velocity = V3 0 150 0
            , rotation = axisAngle (unit _z) (pi/2)
            }
          , target  = Nothing
          , actions = mempty
          , ship    = Ship{ colour = white, armour = 1000, scale = 15 }
          }
        , npcs =
          [ Character
            { actor   = Actor
              { position = P (V3 2500000 0 0)
              , velocity = V3 0 150 0
              , rotation = axisAngle (unit _z) (pi/2)
              }
            , target  = Just (C Player)
            , actions = mempty
            , ship    = Ship{ colour = red, armour = Interval 50 100, scale = 30 }
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
    $ do
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

      start <- now
      evalState start . runStarfield . runShip . runRadar . runLaser . runBody . fix $ \ loop -> do
        continue <- measure "frame" $ do
          t <- realToFrac <$> since epoch
          system <- get
          continue <- execEmpty . runReader (systemAt system (getDelta t)) $ do
            dt <- fmap realToFrac . since =<< get
            put =<< now

            withView (draw dt fpsLabel targetLabel (Font face 18)) -- draw with current readonly positions & beams
            characters_ @Body %= fmap (actor_%~inertia) -- update positions

            measure "input" input
            measure "controls" $ player_ @Body .actions_ <~ controls
            measure "ai" $ npcs_ @Body <~> traverse ai

            -- FIXME: this is so gross
            beams_ @Body .= []
            npcs_ @Body %= filter (\ Character{ ship = Ship{ armour } } -> armour^.min_ > 0)
            characters_ @Body <~> traverse
              (   measure "gravity" . (actor_ @Character <-> gravity dt)
              >=> measure "hit" . hit dt
              >=> measure "runActions" . runActions dt)
          continue <$ measure "swap" Window.swap
        when continue loop

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


execEmpty :: Functor m => EmptyC m a -> m Bool
execEmpty = fmap isJust . runEmpty
