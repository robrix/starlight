{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module Starlight.Game
( game
) where

import           Control.Algebra
import           Control.Carrier.Empty.Church
import           Control.Carrier.Finally
import           Control.Carrier.Reader
import qualified Control.Carrier.State.STM.TVar as TVar
import           Control.Carrier.State.Strict
import           Control.Effect.Lens.Exts as Lens
import           Control.Effect.Profile
import           Control.Effect.Thread
import           Control.Effect.Trace
import           Control.Lens (itraverse, (^.))
import           Control.Monad (unless, (>=>))
import           Control.Monad.IO.Class.Lift
import           Data.Coerce
import           Data.Function (fix)
import           Data.Functor.Identity
import           Data.Functor.Interval
import qualified Data.Map as Map
import           Data.Time.Clock (UTCTime)
import           GL
import           GL.Effect.Check
import           Linear.Exts
import           Starlight.Actor
import           Starlight.AI
import           Starlight.Body
import           Starlight.Character
import           Starlight.Controls
import           Starlight.Draw
import qualified Starlight.Draw.Body as Body
import           Starlight.Draw.Radar as Radar
import           Starlight.Draw.Ship as Ship
import           Starlight.Draw.Starfield as Starfield
import           Starlight.Draw.Weapon.Laser as Laser
import           Starlight.Identifier
import           Starlight.Input
import           Starlight.Physics
import           Starlight.Radar
import           Starlight.Ship
import qualified Starlight.Sol as Sol
import           Starlight.System as System
import           Starlight.Time
import           Starlight.UI
import           Starlight.View
import           System.FilePath
import           UI.Colour
import           UI.Context
import           UI.Label as Label
import           UI.Typeface (cacheCharactersForDrawing, readTypeface)
import qualified UI.Window as Window
import           Unit.Length

runGame
  :: ( Has (Lift IO) sig m
     , MonadFail m
     )
  => System Body
  -> ReaderC Epoch (TVar.StateC Bool (TVar.StateC (System Body) (TVar.StateC Input (FinallyC (GLC (ReaderC Context (ReaderC Window.Window m))))))) a
  -> m a
runGame system
  = Window.runSDL
  . Window.runWindow "Starlight" (V2 1024 768)
  . runContext
  . runGLC
  . runFinally
  . TVar.evalState @Input mempty
  . TVar.evalState system
      { characters = Map.fromList $ zip (Player : map NPC [0..])
        [ Character
          { actor   = Actor
            { position  = V3 2_500 0 0
            , velocity  = V3 0 0 0
            , rotation  = axisAngle (unit _z) (pi/2)
            , mass      = 1000
            , magnitude = 30
            }
          , target  = Nothing
          , actions = mempty
          , ship    = Ship{ colour = white, armour = 1_000, radar }
          }
        , Character
          { actor   = Actor
            { position  = V3 2_500 0 0
            , velocity  = V3 0 0 0
            , rotation  = axisAngle (unit _z) (pi/2)
            , mass      = 1000
            , magnitude = 60
            }
          , target  = Nothing -- Just (C Player)
          , actions = mempty
          , ship    = Ship{ colour = red, armour = Interval 500 500, radar }
          }
        , Character
          { actor   = Actor
            { position  = V3 2_500 0 0
            , velocity  = V3 0 0 0
            , rotation  = axisAngle (unit _z) (pi/2)
            , mass      = 1000
            , magnitude = 30
            }
          , target  = Just $ B (Star (10, "Sol"))
          , actions = mempty
          , ship    = Ship{ colour = white, armour = 1_000, radar }
          }
        , Character
          { actor   = Actor
            { position  = V3 2_500 0 0
            , velocity  = V3 0 0 0
            , rotation  = axisAngle (unit _z) (pi/2)
            , mass      = 1000
            , magnitude = 30
            }
          , target  = Just $ B (Star (10, "Sol") :/ (199, "Mercury"))
          , actions = mempty
          , ship    = Ship{ colour = white, armour = 1_000, radar }
          }
        ]
      }
    . TVar.evalState False
    . runJ2000
    where
  radar = Radar 1000 -- GW radar

runFrame
  :: ( Effect sig
     , Has Check sig m
     , Has Finally sig m
     , Has (Lift IO) sig m
     , Has Trace sig m
     )
  => ReaderC Body.Drawable (ReaderC Laser.Drawable (ReaderC Radar.Drawable (ReaderC Ship.Drawable (ReaderC Starfield.Drawable (StateC UTCTime (EmptyC m)))))) a
  -> m ()
runFrame = evalEmpty . (\ m -> now >>= \ start -> evalState start m) . runStarfield . runShip . runRadar . runLaser . Body.runBody

game
  :: ( Effect sig
     , Has Check sig m
     , Has (Lift IO) sig m
     , Has Profile sig m
     , Has Thread sig m
     , Has Trace sig m
     , MonadFail m
     )
  => m ()
game = Sol.system >>= \ system -> runGame system $ do
  trace "loading typeface"
  face <- measure "readTypeface" $ readTypeface ("fonts" </> "DejaVuSans.ttf")
  measure "cacheCharactersForDrawing" . cacheCharactersForDrawing face $ ['0'..'9'] <> ['a'..'z'] <> ['A'..'Z'] <> "./:-⁻¹·" -- characters to preload

  fpsLabel    <- measure "label" Label.label
  targetLabel <- measure "label" Label.label

  start <- now
  fork . evalState start . fix $ \ loop -> do
    id <~> timed . flip (execState @(System Body)) (measure "integration" (runSystem (do
      measure "controls" $ player_ @Body .actions_ <~ controls
      measure "ai" $ npcs_ @Body <~> traverse ai

      -- FIXME: this is so gross
      beams_ @Body .= []
      npcs_ @Body %= filter (\ Character{ ship = Ship{ armour } } -> armour^.min_ > 0)
      characters_ @Body <~> itraverse
        (\ i
        -> local . neighbourhoodOf @StateVectors
        <*> ( measure "gravity" . (actor_ @Character <-> gravity)
          >=> measure "hit" . hit i
          >=> measure "runActions" . runActions i
          >=> measure "inertia" . (actor_ <-> inertia))))))
    yield
    hasQuit <- get
    unless hasQuit loop

  enabled_ Blend            .= True
  enabled_ DepthClamp       .= True
  enabled_ LineSmooth       .= True
  enabled_ ProgramPointSize .= True
  enabled_ ScissorTest      .= True

  runFrame . runReader UI{ fps = fpsLabel, target = targetLabel, face } . fix $ \ loop -> do
    measure "frame" frame
    measure "swap" Window.swap
    loop
  put True

frame
  :: ( Has Check sig m
     , Has Empty sig m
     , Has (Lift IO) sig m
     , Has Profile sig m
     , Has (Reader Body.Drawable) sig m
     , Has (Reader Laser.Drawable) sig m
     , Has (Reader Radar.Drawable) sig m
     , Has (Reader Ship.Drawable) sig m
     , Has (Reader Starfield.Drawable) sig m
     , Has (Reader Epoch) sig m
     , Has (Reader UI) sig m
     , Has (Reader Window.Window) sig m
     , Has (State Input) sig m
     , Has (State (System Body)) sig m
     , Has (State UTCTime) sig m
     )
  => m ()
frame = runSystem . timed $ do
  measure "input" input
  withView (local (neighbourhoodOfPlayer @StateVectors) draw) -- draw with current readonly positions & beams

-- | Compute the zoom factor for the given velocity.
--
-- Higher values correlate to more of the scene being visible.
zoomForSpeed :: V2 (Window.Pixels Int) -> Float -> Float
zoomForSpeed size x = go where
  Identity go
    | Identity x < min' speed = min' zoom
    | Identity x > max' speed = max' zoom
    | otherwise               = fromUnit zoom (coerce easeInOutCubic (toUnit speed (Identity x)))
  zoom = Interval 1 5
  speed = speedAt <$> zoom
  speedAt x = x * fromIntegral (maximum size) * 2

withView
  :: ( Has (Lift IO) sig m
     , Has (Reader (System StateVectors)) sig m
     , Has (Reader Window.Window) sig m
     )
  => ReaderC View m a
  -> m a
withView m = do
  ratio <- Window.ratio
  size  <- Window.size

  bodies   <- view (bodies_ @StateVectors)
  velocity <- view (player_ @StateVectors .actor_.velocity_)
  focus    <- view (player_ @StateVectors .actor_.position_._xy)

  let zoom = zoomForSpeed size (prj (norm velocity))
      solI = Star (10, "Sol")
      scale = un @(Kilo Metres) (100_000 / radius (body (bodies Map.! solI))) -- FIXME: account for unit size without hard-coding conversion factor
  runReader View{ ratio, size, zoom, scale, focus } m
