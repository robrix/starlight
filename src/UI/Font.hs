{-# LANGUAGE ScopedTypeVariables #-}
module UI.Font where

import Control.Monad (guard)
import qualified Control.Exception as E
import Data.Foldable (find)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Opentype.Fileformat as O

data Typeface = Typeface { typefaceName :: String, typefaceUnderlying :: O.OpentypeFont }

data Font = Font { fontFace :: Typeface, fontSize :: Float }


readTypeface :: FilePath -> IO (Maybe Typeface)
readTypeface path = (toTypeface <$> O.readOTFile path) `E.catch` (\ (E.SomeException _) -> return Nothing)
  where toTypeface font = do
          name <- opentypeFontName font
          pure $ Typeface name font

data NameID
  = Copyright
  | FamilyName
  | SubfamilyName
  | UniqueID
  | FullName
  | Version
  | PostScriptName
  | Trademark
  | ManufacturerName
  | Designer
  | Description
  | VendorURL
  | DesignerURL
  | LicenseDescription
  | LicenseURL
  | Reserved
  | TypographicFamilyName
  | TypographicSubfamilyName
  | CompatibleFullName
  | SampleText
  | PostScriptCIDFindFontName
  | WWSFamilyName
  | WWSSubfamilyName
  | LightBackgroundPalette
  | DarkBackgroundPalette
  | VariationsPostScriptNamePrefix
  deriving (Bounded, Enum, Eq, Ord, Show)

opentypeFontName :: O.OpentypeFont -> Maybe String
opentypeFontName o = T.unpack . T.decodeUtf16BE . O.nameString <$> find ((== Just FullName) . nameID) (O.nameRecords (O.nameTable o))

nameID :: O.NameRecord -> Maybe NameID
nameID = safeToEnum . fromIntegral . O.nameID


safeToEnum :: forall n. (Bounded n, Enum n) => Int -> Maybe n
safeToEnum n = do
  guard (n < fromEnum (maxBound :: n))
  guard (n > fromEnum (minBound :: n))
  pure (toEnum n)
