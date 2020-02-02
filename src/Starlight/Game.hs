{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module Starlight.Game
( game
) where

import           Control.Algebra
import           Control.Carrier.Finally
import           Control.Carrier.Random.Gen
import           Control.Carrier.Reader
import qualified Control.Carrier.State.STM.TVar as TVar
import           Control.Carrier.State.Strict
import           Control.Effect.Lens.Exts as Lens
import           Control.Effect.Profile
import           Control.Effect.Thread
import           Control.Effect.Trace
import           Control.Monad (unless)
import           Control.Monad.IO.Class.Lift
import           Data.Flag
import           Data.Function (fix)
import qualified Data.Map as Map
import           GL
import           GL.Effect.Check
import           Linear.Exts
import qualified SDL
import           Starlight.Actor
import           Starlight.Body
import           Starlight.Character
import           Starlight.Draw
import           Starlight.Identifier
import           Starlight.Input
import           Starlight.Integration
import           Starlight.Physics
import           Starlight.Radar
import           Starlight.Ship
import qualified Starlight.Sol as Sol
import           Starlight.System as System
import           Starlight.Time
import           Starlight.UI
import           Stochastic.Sample.Markov
import           System.FilePath
import           System.Random.SplitMix (SMGen, newSMGen)
import           UI.Colour
import           UI.Context
import           UI.Label as Label
import           UI.Typeface (cacheCharactersForDrawing, readTypeface)
import qualified UI.Window as Window
import           Unit.Count
import           Unit.Length

runGame
  :: ( Effect sig
     , Has (Lift IO) sig m
     , MonadFail m
     )
  => Map.Map BodyIdentifier Body
  -> ReaderC Epoch (StateC (Chain (V2 (Distance Double))) (TVar.StateC (Flag Quit) (TVar.StateC (System Body) (TVar.StateC Input (RandomC SMGen (LiftIO (FinallyC (GLC (ReaderC Context (ReaderC Window.Window m)))))))))) a
  -> m a
runGame bodies
  = Window.runSDL
  . Window.runWindow "Starlight" (V2 1024 768)
  . runContext
  . runGLC
  . runFinally
  . runLiftIO
  . (\ m -> sendM newSMGen >>= flip evalRandom m)
  . TVar.evalState @Input mempty
  . TVar.evalState System
      { bodies
      , players = Map.fromList
        [ (,) (0, "you") $ Character
          { name    = "you"
          , actor   = Actor
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
      , npcs    = mempty
      , beams   = mempty
      }
    . TVar.evalState (toFlag Quit False)
    . evalState (Chain (0 :: V2 (Distance Double)))
    . runJ2000
    where
  magnitude :: Metres Double
  magnitude = 500
    -- stem-to-stern length; currently interpreted as “diameter” for hit testing
    -- compare: USS Gerald R. Ford is 337m long
  start :: V3 (Mega Metres Double)
  start = V3 2_500 0 0
  radar = Radar 1000 -- GW radar

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
game = Sol.bodiesFromSQL >>= \ bodies -> runGame bodies $ do
  SDL.cursorVisible SDL.$= False
  trace "loading typeface"
  face <- measure "readTypeface" $ readTypeface ("fonts" </> "DejaVuSans.ttf")
  measure "cacheCharactersForDrawing" . cacheCharactersForDrawing face $ ['0'..'9'] <> ['a'..'z'] <> ['A'..'Z'] <> "./:-⁻⁰¹²³⁴⁵⁶⁷⁸⁹·" -- characters to preload

  fpsLabel    <- measure "label" Label.label
  targetLabel <- measure "label" Label.label

  start <- now
  fork . evalState start . fix $ \ loop -> do
    id <~> integration
    yield
    hasQuit <- gets (fromFlag Quit)
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
  put (toFlag Quit True)


-- | Flag parameter indicating the meaning of the signal to quit the threads.
data Quit = Quit
