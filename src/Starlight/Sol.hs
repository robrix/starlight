{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
-- | A familiar star system.
module Starlight.Sol
( runData
, loadBodies
, loadFactions
) where

import           Control.Carrier.Database.SQLite
import           Control.Effect.Lift
import           Control.Lens (review)
import           Control.Monad.Fix
import qualified Data.IntMap as IntMap
import qualified Data.Map as Map
import           Database.SQLite3 (SQLData(..))
import           Linear.Exts
import           Paths_starlight
import           Starlight.Body
import           Starlight.Faction
import           Starlight.Identifier
import           UI.Colour
import           Unit.Angle
import           Unit.Length
import           Unit.Mass
import           Unit.Time

runData :: Has (Lift IO) sig m => DatabaseC m a -> m a
runData m = sendM (getDataFileName "data/data.db") >>= \ file -> runDatabase file m

loadBodies :: (HasLabelled Database (Database stmt) sig m, MonadFail m, MonadFix m) => m (Map.Map BodyIdentifier Body)
loadBodies = execute "select rowid, * from bodies" $ \ stmt -> do
  entries <- mfix $ \ ephemerides -> fix (\ loop elems -> do
    res <- step stmt
    case res of
      Nothing   -> pure elems
      Just [ SQLInteger rowid, parentId, SQLInteger code, SQLText name, SQLInteger population, SQLFloat radius, SQLFloat mass, SQLFloat tilt, SQLFloat rotationalPeriod, SQLInteger colour, SQLFloat eccentricity, SQLFloat semimajor, SQLFloat longitudeOfAscendingNode, SQLFloat inclination, SQLFloat argumentOfPerifocus, SQLFloat orbitalPeriod, SQLFloat timeOfPeriapsis ] -> do
        let leaf = (fromIntegral code, name)
            identifier = maybe (Star leaf) (:/ leaf) (lookupParent ephemerides parentId)
            entry = Body
              { population      = fromIntegral population
              , radius          = pure @(Kilo Metres) radius
              , mass            = pure @(Kilo Grams)  mass
              , rotation        = Revolution
                { orientation = axisAngle (unit _x) (convert @Degrees (pure tilt))
                , period      = convert @Days @Seconds (pure rotationalPeriod)
                }
              , eccentricity    = I eccentricity
              , semimajor       = pure @(Kilo Metres) semimajor
              , revolution      = Revolution
                { orientation = orient
                  (convert @Degrees (pure longitudeOfAscendingNode))
                  (convert @Degrees (pure inclination))
                  (convert @Degrees (pure argumentOfPerifocus))
                , period      = pure @Seconds orbitalPeriod
                }
              , timeOfPeriapsis = pure @Seconds timeOfPeriapsis
              , colour          = review packed (fromIntegral colour)
              }
        loop (IntMap.insert (fromIntegral rowid) (identifier, entry) elems)
      row -> fail $ "loadBodies.bodies: bad row: " <> show row) IntMap.empty
  pure $! Map.fromList (IntMap.elems entries)
  where
  lookupParent ephemerides = \case
    SQLInteger i -> fst <$> IntMap.lookup (fromIntegral i) ephemerides
    _            -> Nothing

loadFactions :: (HasLabelled Database (Database stmt) sig m, MonadFail m) => m Factions
loadFactions = do
  rs <- execute "select * from relationships" $ \ stmt -> fix (\ loop elems -> do
    res <- step stmt
    case res of
      Nothing -> pure elems
      Just [ SQLInteger faction1Id, SQLInteger faction2Id, SQLFloat rel ] ->
        loop (IntMap.insertWith (<>) (fromIntegral faction1Id) (IntMap.singleton (fromIntegral faction2Id) rel) elems)
      Just row -> fail $ "loadFactions.relationships: bad row: " <> show row) IntMap.empty

  fs <- execute "select rowid, * from factions" $ \ stmt -> fix (\ loop elems -> do
    res <- step stmt
    case res of
      Nothing -> pure elems
      Just [ SQLInteger rowid, SQLText name, SQLInteger colour ] ->
        loop (IntMap.insert (fromIntegral rowid) (Faction name (review packed (fromIntegral colour)) (IntMap.toList (rs IntMap.! fromIntegral rowid))) elems)
      Just row -> fail $ "loadFactions.factions: bad row: " <> show row) IntMap.empty
  pure $ factions fs
