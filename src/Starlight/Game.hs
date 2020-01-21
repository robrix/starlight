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
import           Data.Function (fix)
import           Data.Functor.Interval
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
import qualified Starlight.Draw.Radar as Radar
import qualified Starlight.Draw.Ship as Ship
import qualified Starlight.Draw.Starfield as Starfield
import qualified Starlight.Draw.Weapon.Laser as Laser
import           Starlight.Identifier
import           Starlight.Input
import           Starlight.Physics
import           Starlight.Physics.Constants
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
import           Unit.Algebra
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
      { players =
        [ Character
          { actor   = Actor
            { position  = convert <$> start
            , velocity  = 0
            , rotation  = axisAngle (unit _z) (pi/2)
            , mass      = 1000
            , magnitude = convert magnitude
            }
          , target  = Nothing
          , actions = mempty
          , ship    = Ship{ colour = white, armour = 1_000, radar }
          }
        ]
      , npcs    =
        [ Character
          { actor   = Actor
            { position  = convert <$> start
            , velocity  = 0
            , rotation  = axisAngle (unit _z) (pi/2)
            , mass      = 1000
            , magnitude = convert magnitude
            }
          , target  = Nothing -- Just (C Player)
          , actions = mempty
          , ship    = Ship{ colour = red, armour = Interval 500 500, radar }
          }
        , Character
          { actor   = Actor
            { position  = convert <$> start
            , velocity  = 0
            , rotation  = axisAngle (unit _z) (pi/2)
            , mass      = 1000
            , magnitude = convert magnitude
            }
          , target  = Just $ B (Star (10, "Sol"))
          , actions = mempty
          , ship    = Ship{ colour = white, armour = 1_000, radar }
          }
        , Character
          { actor   = Actor
            { position  = convert <$> start
            , velocity  = 0
            , rotation  = axisAngle (unit _z) (pi/2)
            , mass      = 1000
            , magnitude = convert magnitude
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
  magnitude :: Metres Double
  magnitude = 500
    -- stem-to-stern length; currently interpreted as “diameter” for hit testing
    -- compare: USS Gerald R. Ford is 337m long
  start :: V3 (Mega Metres Double)
  start = V3 2_500 0 0
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
runFrame = evalEmpty . (\ m -> now >>= \ start -> evalState start m) . Starfield.run . Ship.run . Radar.run . Laser.run . Body.run

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
  measure "cacheCharactersForDrawing" . cacheCharactersForDrawing face $ ['0'..'9'] <> ['a'..'z'] <> ['A'..'Z'] <> "./:-⁻⁰¹²³⁴⁵⁶⁷⁸⁹·" -- characters to preload

  fpsLabel    <- measure "label" Label.label
  targetLabel <- measure "label" Label.label

  start <- now
  fork . evalState start . fix $ \ loop -> do
    integration
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

integration
  :: ( Effect sig
     , Has (Lift IO) sig m
     , Has Profile sig m
     , Has (Reader Epoch) sig m
     , Has (State Input) sig m
     , Has (State UTCTime) sig m
     , Has (State (System Body)) sig m
     )
  => m ()
integration = id <~> timed . flip (execState @(System Body)) (measure "integration" (runSystem (do
  measure "controls" $ player_ @Body .actions_ <~ controls
  measure "ai" $ npcs_ @Body <~> traverse ai

  -- FIXME: this is so gross
  beams_ @Body .= []
  npcs_ @Body %= filter ((> 0) . (^.ship_.armour_.min_))
  characters_ @Body <~> itraverse
    (\ i
    -> local . neighbourhoodOf @StateVectors
    <*> ( measure "gravity" . (actor_ @Character <-> gravity)
      >=> measure "hit" . hit i
      >=> measure "runActions" . runActions i
      >=> measure "inertia" . (actor_ <-> inertia))))))

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

  velocity <- view (player_ @StateVectors .velocity_)
  focus    <- view (player_ @StateVectors .position_._xy)

  let zoom = zoomForSpeed size (norm velocity)
  runReader View{ ratio, size, zoom, scale = Starlight.Game.scale, shipScale = Starlight.Game.shipScale, focus } m


scale :: (Window.Pixels :/: Distance) Double
scale = Window.Pixels 695_500 ./. convert @(Kilo Metres) @Distance 695_500.0
  -- how many pixels to draw something / the radius of the sun

-- FIXME: this is really stupid; there *has* to be a better way to say “I want a 500 m ship to be 30 px long” or w/e
shipScale :: I Double
shipScale = 30
