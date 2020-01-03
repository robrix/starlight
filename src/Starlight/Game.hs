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
( game
) where

import           Control.Algebra
import           Control.Carrier.Empty.Maybe
import           Control.Carrier.Finally
import           Control.Carrier.Reader
import           Control.Carrier.State.Strict
import           Control.Effect.Lens.Exts as Lens
import           Control.Effect.Profile
import           Control.Effect.Trace
import           Control.Monad (when)
import           Control.Monad.IO.Class.Lift
import           Data.Coerce
import           Data.Foldable (traverse_)
import           Data.Function (fix, (&))
import           Data.Functor.Identity
import           Data.Functor.Interval
import           Data.List.NonEmpty (NonEmpty(..))
import           Data.Maybe (isJust)
import           Data.Time.Clock (NominalDiffTime, UTCTime, diffUTCTime, getCurrentTime)
import           GL
import           Lens.Micro (Lens', lens, (.~), (^.))
import           Linear.Exts
import           Starlight.Actor
import           Starlight.AI
import           Starlight.Body
import           Starlight.Controls
import           Starlight.Draw
import           Starlight.Identifier
import           Starlight.Input
import           Starlight.Physics
import           Starlight.Player
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
    . evalState GameState
      { player = Player
        { actor    = Actor
          { position = P (V2 250000 0)
          , velocity = V2 0 150
          , rotation = axisAngle (unit _z) (pi/2)
          , target   = Nothing
          , health   = 1000
          , actions  = mempty
          }
        }
      , beams  = []
      , system = system
        { actors =
          [ Actor
            { position = P (V2 250000 0)
            , velocity = V2 0 150
            , rotation = axisAngle (unit _z) (pi/2)
            , target   = Nothing
            , health   = 100
            , actions  = mempty
            }
          , Actor
            { position = P (V2 250000 0)
            , velocity = V2 0 150
            , rotation = axisAngle (unit _z) (pi/2)
            , target   = Just $ B (Star (10, "Sol"))
            , health   = 100
            , actions  = mempty
            }
          , Actor
            { position = P (V2 250000 0)
            , velocity = V2 0 150
            , rotation = axisAngle (unit _z) (pi/2)
            , target   = Just $ B (Star (10, "Sol") :/ (199, "Mercury"))
            , health   = 100
            , actions  = mempty
            }
          ]
        }
      } $ do
      face <- measure "readTypeface" $ readTypeface ("fonts" </> "DejaVuSans.ttf")
      measure "cacheCharactersForDrawing" . cacheCharactersForDrawing face $ ['0'..'9'] <> ['a'..'z'] <> ['A'..'Z'] <> "./:-" -- characters to preload

      fpsLabel    <- measure "label" Label.label
      targetLabel <- measure "label" Label.label

      enabled_ Blend            .= True
      enabled_ DepthClamp       .= True
      enabled_ LineSmooth       .= True
      enabled_ ProgramPointSize .= True
      enabled_ ScissorTest      .= True

      start <- now
      evalState start . runStarfield . runShip . runRadar . runLaser . runBody . fix $ \ loop -> do
        continue <- measure "frame" $ do
          t <- realToFrac <$> since start
          system <- use system_
          continue <- evalEmpty . runReader (systemAt system (getDelta t)) $ do
            measure "input" input
            dt <- fmap realToFrac . since =<< get
            put =<< now
            measure "controls" $ Lens.zoom (player_ . actor_ . actions_) (controls >>= put)
            system <- ask
            measure "ai" (zoomEach npcs_ (get >>= ai system >>= assign actions_))
            measure "physics" (zoomEach Starlight.Game.actors_ (modify (physics dt system) >> use actions_ >>= traverse_ (runAction dt)))
            gameState <- get
            withView gameState (draw dt fpsLabel targetLabel (Font face 18) (player gameState))
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
     , Has (Reader Window.Window) sig m
     )
  => GameState
  -> ReaderC View m a
  -> m a
withView game m = do
  scale <- Window.scale
  size  <- Window.size
  let velocity = game ^. player_ . actor_ . velocity_
      zoom     = zoomForSpeed size (norm velocity)
      focus    = game ^. player_ . actor_ . position_
  runReader View{ scale, size, zoom, focus } m

data GameState = GameState
  { player :: !Player
  , beams  :: ![Beam]
  , system :: !(System Body)
  }
  deriving (Show)

player_ :: Lens' GameState Player
player_ = lens player (\ s p -> s { player = p })

npcs_ :: Lens' GameState [Actor]
npcs_ = system_ . System.actors_

actors_ :: Lens' GameState (NonEmpty Actor)
actors_ = lens get set where
  get s = s ^. player_ . actor_ :| s ^. npcs_
  set s (a:|o) = s & player_ . actor_ .~ a & npcs_ .~ o

system_ :: Lens' GameState (System Body)
system_ = lens system (\ s p -> s { system = p })


now :: Has (Lift IO) sig m => m UTCTime
now = sendM getCurrentTime

since :: Has (Lift IO) sig m => UTCTime -> m NominalDiffTime
since t = flip diffUTCTime t <$> now


evalEmpty :: Functor m => EmptyC m a -> m Bool
evalEmpty = fmap isJust . runEmpty
