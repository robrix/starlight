{-# LANGUAGE NamedFieldPuns #-}
module Starlight.CLI
( Options(..)
, profile_
, argumentsParser
  -- * Re-exports
, execParser
) where

import           Data.Version (showVersion)
import           Lens.Micro (Lens', lens)
import           Options.Applicative
import qualified Paths_starlight as Library (version)

data Options = Options
  { profile :: Bool
  }

profile_ :: Lens' Options Bool
profile_ = lens profile (\ o profile -> o { profile })


argumentsParser :: ParserInfo Options
argumentsParser = info
  (version <*> helper <*> options)
  (  fullDesc
  <> progDesc "Starlight is a game about spaceships in space."
  <> header   "Starlight - spaceships in space")

options :: Parser Options
options = Options <$> switch (long "profile" <> help "run with profiling enabled")


versionString :: String
versionString = "starlight version " <> showVersion Library.version

version :: Parser (a -> a)
version = infoOption versionString (long "version" <> short 'V' <> help "Output version info.")
