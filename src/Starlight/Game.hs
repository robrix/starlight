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
import           Control.Lens (to)
import           Control.Monad (when)
import           Control.Monad.IO.Class.Lift
import           Data.Coerce
import           Data.Foldable (traverse_)
import           Data.Function (fix)
import           Data.Functor.Identity
import           Data.Functor.Interval
import           Data.Maybe (isJust)
import           Data.Time.Clock (NominalDiffTime, UTCTime, diffUTCTime, getCurrentTime)
import           Data.Time.Format.ISO8601
import           GL
import           Linear.Exts
import           Starlight.Actor
import           Starlight.AI
import           Starlight.Body
import           Starlight.Character
import           Starlight.Controls
import           Starlight.Draw
import           Starlight.Identifier
import           Starlight.Input
import           Starlight.Physics
import           Starlight.Radar
import           Starlight.Ship
import qualified Starlight.Sol as Sol
import           Starlight.Starfield
import           Starlight.System as System
import           Starlight.View
import           Starlight.Weapon.Laser
import           System.FilePath
import           UI.Label as Label
import           UI.Typeface (Font(Font), cacheCharactersForDrawing, readTypeface)
import qualified UI.Window as Window
import           Unit.Length

game
  :: ( Effect sig
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
          { actor    = Actor
            { position = P (V3 2500000 0 0)
            , velocity = V3 0 150 0
            , rotation = axisAngle (unit _z) (pi/2)
            }
          , target   = Nothing
          , actions  = mempty
          }
        , npcs =
          [ Character
            { actor = Actor
              { position = P (V3 2500000 0 0)
              , velocity = V3 0 150 0
              , rotation = axisAngle (unit _z) (pi/2)
              }
            , target   = Nothing
            , actions  = mempty
            }
          , Character
            { actor = Actor
              { position = P (V3 2500000 0 0)
              , velocity = V3 0 150 0
              , rotation = axisAngle (unit _z) (pi/2)
              }
            , target   = Just $ B (Star (10, "Sol"))
            , actions  = mempty
            }
          , Character
            { actor = Actor
              { position = P (V3 2500000 0 0)
              , velocity = V3 0 150 0
              , rotation = axisAngle (unit _z) (pi/2)
              }
            , target   = Just $ B (Star (10, "Sol") :/ (199, "Mercury"))
            , actions  = mempty
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
          continue <- evalEmpty . runReader (systemAt system (getDelta t)) $ do
            measure "input" input
            dt <- fmap realToFrac . since =<< get
            put =<< now
            measure "controls" $ player_ @Body .actions_ <~ controls
            measure "ai" (zoomEach (npcs_ @Body) ai)
            measure "physics" $
              characters_ @Body `zoomEach` do
                actions <- use actions_
                traverse_ (runAction dt) actions
                actor_ @Character `Lens.zoom` physics dt
            withView (draw dt fpsLabel targetLabel (Font face 18))
          continue <$ measure "swap" Window.swap
        when continue loop

-- | Compute the zoom factor for the given velocity.
--
-- Higher values correlate to more of the scene being visible.
zoomForSpeed :: V2 Int -> Float -> Float
zoomForSpeed size x = runIdentity go where
  go
    | Identity x < min_ speed = min_ zoom
    | Identity x > max_ speed = max_ zoom
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

  let zoom     = zoomForSpeed size (norm velocity)
  runReader View{ scale, size, zoom, focus } m


now :: Has (Lift IO) sig m => m UTCTime
now = sendM getCurrentTime

since :: Has (Lift IO) sig m => UTCTime -> m NominalDiffTime
since t = flip diffUTCTime t <$> now


evalEmpty :: Functor m => EmptyC m a -> m Bool
evalEmpty = fmap isJust . runEmpty
