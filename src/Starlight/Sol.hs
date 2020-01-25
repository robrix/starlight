{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
-- | A familiar star bodies.
module Starlight.Sol
( bodiesFromSQL
) where

import           Control.Effect.Lift
import           Control.Exception.Lift
import           Control.Lens (review)
import           Control.Monad.Fix
import qualified Data.Map as Map
import           Data.Text (pack)
import           Database.SQLite3
import           Linear.Exts
import           Paths_starlight
import           Starlight.Body
import           Starlight.Identifier
import           UI.Colour
import           Unit.Angle
import           Unit.Length
import           Unit.Mass
import           Unit.Time

bodiesFromSQL :: Has (Lift IO) sig m => m (Map.Map BodyIdentifier Body)
bodiesFromSQL = sendM (getDataFileName "ephemerides/ephemerides.db") >>= \ file -> bracket (sendM (open (pack file))) (sendM . close) $ \ db -> sendM $ do
  stmt <- prepare db "select rowid, * from bodies"
  entries <- mfix $ \ ephemerides -> fix (\ loop elems -> do
    res <- step stmt
    cols <- columns stmt
    case res of
      Done -> pure (elems [])
      Row  -> do
        entry <- fromColumns ephemerides cols
        loop (elems . (entry:))) id
  finalize stmt
  pure $! Map.fromList (map snd entries)
  where
  fromColumns ephemerides = \case
    [ SQLInteger rowid, parentId, SQLInteger code, SQLText name, SQLFloat radius, SQLFloat mass, SQLFloat tilt, SQLFloat period, SQLInteger colour, SQLFloat eccentricity, SQLFloat semimajor, SQLFloat longitudeOfAscendingNode, SQLFloat inclination, SQLFloat argumentOfPerifocus, SQLFloat orbitalPeriod, SQLFloat timeOfPeriapsis ] -> do
      let leaf = (fromIntegral code, name)
          identifier = maybe (Star leaf) (:/ leaf) (lookupParent ephemerides parentId)
      pure (rowid, (identifier, Body
        { radius = pure @(Kilo Metres) radius
        , mass   = pure @(Kilo Grams)  mass
        , tilt   = pure @Degrees       tilt
        , period = convert @Days @Seconds (pure period)
        , colour = review packed (fromIntegral colour)
        , orbit  = Orbit
          { eccentricity    = I eccentricity
          , semimajor       = pure @(Kilo Metres) semimajor
          , orientation     = orient
            (convert @Degrees (pure longitudeOfAscendingNode))
            (convert @Degrees (pure inclination))
            (convert @Degrees (pure argumentOfPerifocus))
          , period          = pure @Seconds orbitalPeriod
          , timeOfPeriapsis = pure @Seconds timeOfPeriapsis
          }
        }))
    row -> fail $ "bad row: " <> show row
  lookupParent ephemerides = \case
    SQLInteger i -> fst <$> lookup i ephemerides
    _            -> Nothing
