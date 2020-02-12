module Data.SQL
( runDatabase
) where

import Control.Carrier.Reader
import Control.Effect.Lift
import Control.Exception.Lift
import Data.Text (pack)
import Database.SQLite3

runDatabase :: Has (Lift IO) sig m => FilePath -> ReaderC Database m a -> m a
runDatabase file m = bracket (sendM (open (pack file))) (sendM . close) (`runReader` m)
