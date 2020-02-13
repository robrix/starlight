module Control.Carrier.Database.SQLite
( -- * Database carrier
  DatabaseC(..)
) where

import           Control.Carrier.Reader
import qualified Database.SQLite3 as SQLite

newtype DatabaseC m a = DatabaseC (ReaderC SQLite.Database m a)
