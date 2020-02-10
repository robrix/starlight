{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}
module Starlight.CLI
( Options(..)
, ShouldProfile(..)
, ShouldTrace(..)
, ShouldCheck(..)
, defaultOptions
, profile_
, trace_
, check_
, argumentsParser
  -- * Re-exports
, execParser
) where

import           Control.Lens
import           Data.Flag
import           Data.Foldable (foldl')
import           Data.Generics.Product.Fields
import           Data.Version (showVersion)
import           GHC.Generics (Generic)
import           Options.Applicative
import qualified Paths_starlight as Library (version)

data Options = Options
  { profile :: Flag ShouldProfile
  , trace   :: Flag ShouldTrace
  , check   :: Flag ShouldCheck
  }
  deriving (Generic, Show)

data ShouldProfile = ShouldProfile
data ShouldTrace   = ShouldTrace
data ShouldCheck   = ShouldCheck

defaultOptions :: Options
defaultOptions = Options
  { profile = toFlag ShouldProfile False
  , trace   = toFlag ShouldTrace   False
  , check   = toFlag ShouldCheck   False
  }

profile_ :: Lens' Options (Flag ShouldProfile)
profile_ = field @"profile"

trace_ :: Lens' Options (Flag ShouldTrace)
trace_ = field @"trace"

check_ :: Lens' Options (Flag ShouldCheck)
check_ = field @"check"


argumentsParser :: ParserInfo Options
argumentsParser = info
  (version <*> helper <*> options)
  (  fullDesc
  <> progDesc "Starlight is a game about spaceships in space."
  <> header   "Starlight - spaceships in space")

options :: Parser Options
options = foldl' (&) defaultOptions <$> sequenceA
  [ flag id (profile_ .~ toFlag ShouldProfile True) (long "profile" <> help "run with profiling enabled")
  , flag id (trace_   .~ toFlag ShouldTrace   True) (long "trace"   <> help "run with tracing enabled")
  , flag id (check_   .~ toFlag ShouldCheck   True) (long "check"   <> help "run with error checking enabled")
  ]


versionString :: String
versionString = "starlight version " <> showVersion Library.version

version :: Parser (a -> a)
version = infoOption versionString (long "version" <> short 'V' <> help "Output version info.")
