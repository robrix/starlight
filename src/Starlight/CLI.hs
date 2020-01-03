{-# LANGUAGE NamedFieldPuns #-}
module Starlight.CLI
( Options(..)
, defaultOptions
, profile_
, trace_
, argumentsParser
  -- * Re-exports
, execParser
) where

import           Control.Lens
import           Data.Foldable (foldl')
import           Data.Version (showVersion)
import           Options.Applicative
import qualified Paths_starlight as Library (version)

data Options = Options
  { profile :: Bool
  , trace   :: Bool
  }

defaultOptions :: Options
defaultOptions = Options
  { profile = False
  , trace   = False
  }

profile_ :: Lens' Options Bool
profile_ = lens profile (\ o profile -> o { profile })

trace_ :: Lens' Options Bool
trace_ = lens trace (\ o trace -> o { trace })


argumentsParser :: ParserInfo Options
argumentsParser = info
  (version <*> helper <*> options)
  (  fullDesc
  <> progDesc "Starlight is a game about spaceships in space."
  <> header   "Starlight - spaceships in space")

options :: Parser Options
options = foldl' (&) defaultOptions <$> sequenceA
  [ flag id (profile_ .~ True) (long "profile" <> help "run with profiling enabled")
  , flag id (trace_   .~ True) (long "trace"   <> help "run with tracing enabled")
  ]


versionString :: String
versionString = "starlight version " <> showVersion Library.version

version :: Parser (a -> a)
version = infoOption versionString (long "version" <> short 'V' <> help "Output version info.")
