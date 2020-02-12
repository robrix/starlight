{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
-- | A familiar star system.
module Starlight.Sol
( bodiesFromSQL
) where

import           Control.Effect.Lift
import           Control.Effect.Reader
import           Control.Lens (review)
import           Control.Monad.Fix
import qualified Data.Map as Map
import           Data.SQL
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

bodiesFromSQL :: (Has (Lift IO) sig m, MonadFail m, MonadFix m) => m (Map.Map BodyIdentifier Body)
bodiesFromSQL = sendM (getDataFileName "data/data.db") >>= \ file -> runDatabase file $ do
  db <- ask
  stmt <- sendM (prepare db "select rowid, * from bodies")
  entries <- mfix $ \ ephemerides -> fix (\ loop elems -> do
    res <- sendM (step stmt)
    cols <- sendM (columns stmt)
    case res of
      Done -> pure (elems [])
      Row  -> do
        entry <- fromColumns ephemerides cols
        loop (elems . (entry:))) id
  sendM (finalize stmt)
  pure $! Map.fromList (map snd entries)
  where
  fromColumns ephemerides = \case
    [ SQLInteger rowid, parentId, SQLInteger code, SQLText name, SQLInteger population, SQLFloat radius, SQLFloat mass, SQLFloat tilt, SQLFloat rotationalPeriod, SQLInteger colour, SQLFloat eccentricity, SQLFloat semimajor, SQLFloat longitudeOfAscendingNode, SQLFloat inclination, SQLFloat argumentOfPerifocus, SQLFloat orbitalPeriod, SQLFloat timeOfPeriapsis ] -> do
      let leaf = (fromIntegral code, name)
          identifier = maybe (Star leaf) (:/ leaf) (lookupParent ephemerides parentId)
      pure (rowid, (identifier, Body
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
        }))
    row -> fail $ "bad row: " <> show row
  lookupParent ephemerides = \case
    SQLInteger i -> fst <$> lookup i ephemerides
    _            -> Nothing
