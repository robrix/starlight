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
import           Control.Lens (itraverse, (^.))
import           Control.Monad (unless, when, (>=>))
import           Control.Monad.IO.Class.Lift
import           Data.Flag
import           Data.Function (fix)
import           Data.Functor.Interval
import qualified Data.Map as Map
import qualified Data.Text as Text
import           Data.Time.Clock (UTCTime)
import           Geometry.Circle
import           GL
import           GL.Effect.Check
import           Linear.Exts
import qualified SDL
import           Starlight.Actor
import           Starlight.AI
import           Starlight.Body
import           Starlight.Character
import           Starlight.Controls
import           Starlight.Draw
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
import           Stochastic.PDF
import           Stochastic.Sample.Markov
import           Stochastic.Sample.Slice
import           System.FilePath
import           System.Random.SplitMix (SMGen, newSMGen)
import           UI.Colour
import           UI.Context
import           UI.Label as Label
import           UI.Typeface (cacheCharactersForDrawing, readTypeface)
import qualified UI.Window as Window
import           Unit.Algebra
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
    integration
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

spawnPDF :: Has (Reader (System StateVectors)) sig m => m (PDF (V2 (Distance Double)) ((Count "population" :/: Giga Metres :^: 2) Double))
spawnPDF = views (bodies_ @StateVectors) (nearBody . (Map.! (Star (10, "Sol") :/ (399, "Terra"))))

pickSpawnPoint
  :: ( Has Random sig m
     , Has (State (Chain (V2 (Distance Double)))) sig m
     )
  => PDF (V2 (Distance Double)) ((Count "population" :/: Giga Metres :^: 2) Double)
  -> m (V3 (Distance Double))
pickSpawnPoint pdf = do
  let mx = convert @(Kilo Metres) @Distance 6e9
  (`ext` 0) <$> sample (interval 0 1) (interval (-mx) mx) pdf

nearBody :: StateVectors -> PDF (V2 (Distance Double)) ((Count "population" :/: Giga Metres :^: 2) Double)
nearBody sv = PDF pdf
  where
  pdf v
    | let qdV = v `qdU` (sv^.position_._xy)
    , qdV .>. sqU radius = population ./. qdV
    | otherwise          = 0
  radius = sv^.body_.radius_
  population = Count @"population" (prj (I (10_000_000_000 / 1_000_000) ./. a)) -- assume one in a million people actually goes to space
  a = area radius

npc
  :: Text.Text
  -> V3 (Distance Double)
  -> Character
npc name position = Character
  { name
  , actor   = Actor
    { position
    , velocity  = 0
    , rotation  = axisAngle (unit _z) (pi/2)
    , mass      = 1000
    , magnitude = convert (Metres 500)
    }
  , target  = Nothing
  , actions = mempty
  , ship    = Ship{ colour = red, armour = Interval 500 500, radar = Radar 1000 }
  }

-- FIXME: do something clever, more generative
pickName :: (Has (Lift IO) sig m, Has Random sig m) => m Text.Text
pickName = do
  names <- lines <$> sendM (readFile "data/ship-names.txt")
  i <- uniformR (0, pred (length names))
  pure $! Text.pack (names !! i)

integration
  :: ( Effect sig
     , Has (Lift IO) sig m
     , Has Profile sig m
     , Has Random sig m
     , Has (Reader Epoch) sig m
     , Has (State (Chain (V2 (Distance Double)))) sig m
     , Has (State Input) sig m
     , Has (State UTCTime) sig m
     , Has (State (System Body)) sig m
     )
  => m ()
integration = id <~> timed . flip (execState @(System Body)) (measure "integration" (runSystem (do
  measure "controls" $ player_ @Body .actions_ <~ controls
  measure "ai" $ npcs_ @Body <~> traverse ai

  playerPos <- use (player_ @Body .position_)
  let radius = 1
  nearbyNPCs <- uses (npcs_ @Body) (Count @"population" . fromIntegral . length . filter ((< radius) . distance playerPos . (^.position_)) . Map.elems)
  pdf <- spawnPDF
  when (nearbyNPCs ./. area radius < runPDF pdf (playerPos^._xy)) $ do
    pos <- pickSpawnPoint pdf
    when (distance pos playerPos < 1) $ do
      npc <- npc <$> pickName <*> pure pos
      npcs_ @Body %= Map.insert (0, name npc) npc

  -- FIXME: this is so gross
  beams_ @Body .= []
  npcs_ @Body %= Map.filter ((> 0) . (^.ship_.armour_.min_))
  characters_ @Body <~> itraverse
    (\ i
    -> local . neighbourhoodOf @StateVectors
    <*> ( measure "gravity" . (actor_ @Character <-> gravity)
      >=> measure "hit" . hit i
      >=> measure "runActions" . runActions i
      >=> measure "inertia" . (actor_ <-> inertia))))))


-- | Flag parameter indicating the meaning of the signal to quit the threads.
data Quit = Quit
