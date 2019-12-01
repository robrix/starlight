{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
module UI.Font
( Typeface(..)
, Font(..)
, readTypeface
, unitsPerEm
, ascent
, descent
, glyphs
) where

import Control.Applicative (liftA2)
import Control.Monad (guard)
import qualified Control.Exception as E
import Data.Bifunctor (first)
import Data.Char (ord)
import Data.Foldable (find)
import Data.Int
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Vector ((!?))
import Data.Word
import Geometry.Rect
import Geometry.Triangle
import qualified Opentype.Fileformat as O
import Lens.Micro
import Linear.V2
import Linear.V4
import UI.Glyph
import UI.Path

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


unitsPerEm :: Typeface -> Word16
unitsPerEm = O.unitsPerEm . O.headTable . typefaceUnderlying

ascent :: Typeface -> Int16
ascent = O.ascent . O.hheaTable . typefaceUnderlying

descent :: Typeface -> Int16
descent = O.descent . O.hheaTable . typefaceUnderlying


glyphs :: Typeface -> [Char] -> [Glyph]
glyphs typeface chars = concat (zipWith toGlyph chars (glyphsForChars typeface chars))
  where toGlyph char (Just g) = let vertices = glyphVertices typeface g in
          [ scaleGlyph (V2 (1 / fromIntegral (unitsPerEm typeface)) (1 / fromIntegral (unitsPerEm typeface))) $ Glyph char (fromIntegral (O.advanceWidth g)) vertices (bounds ((^. _xy) <$> vertices)) ]
        toGlyph _ Nothing = []
        bounds vertices = Rect (foldr1 (liftA2 min) vertices) (foldr1 (liftA2 max) vertices)

glyphsForChars :: Typeface -> [Char] -> [Maybe (O.Glyph Int)]
glyphsForChars (Typeface _ o) chars = map (>>= (glyphs !?) . fromIntegral) glyphIDs
  where glyphIDs = fromMaybe (Nothing <$ chars) $ do
          cmap <- find viablePlatform (O.getCmaps (O.cmapTable o))
          Just $ lookupAll (O.glyphMap cmap) (fmap (fromIntegral . ord :: Char -> Word32) chars)
        lookupAll = fmap . flip Map.lookup
        glyphs = case O.outlineTables o of
          O.QuadTables _ (O.GlyfTable glyphs) -> glyphs
          _ -> error "wtf"
        viablePlatform p = O.cmapPlatform p == O.UnicodePlatform || O.cmapPlatform p == O.MicrosoftPlatform && O.cmapEncoding p == 1


contourToPath :: [O.CurvePoint] -> Path V2 O.FWord
contourToPath [] = Z
contourToPath (p@(O.CurvePoint x y _) : ps) = M (V2 x y) (go p ps)
  where go (O.CurvePoint _  _  True)  (p@(O.CurvePoint _  _  False) : ps) = go p ps
        go (O.CurvePoint _  _  True)  (p@(O.CurvePoint x  y  True)  : ps) = L (V2 x y) (go p ps)
        go (O.CurvePoint x1 y1 False) (p@(O.CurvePoint x2 y2 False) : ps) = Q (V2 x1 y1) (V2 (x1 + ((x2 - x1) `div` 2)) (y1 + ((y2 - y1) `div` 2))) (go p ps)
        go (O.CurvePoint x1 y1 False) (p@(O.CurvePoint x2 y2 True)  : ps) = Q (V2 x1 y1) (V2 x2 y2) (go p ps)
        go (O.CurvePoint x1 y1 False) []                                  = Q (V2 x1 y1) (V2 x y) Z
        go _                          []                                  = Z

glyphPaths :: Typeface -> O.Glyph Int -> [Path V2 O.FWord]
glyphPaths typeface glyph = fmap contourToPath (O.getScaledContours (typefaceUnderlying typeface) glyph)


glyphVertices :: Typeface -> O.Glyph Int -> [V4 Float]
glyphVertices typeface = (>>= uncurry triangleVertices . first (fmap fromIntegral)) . (>>= pathTriangles) . glyphPaths typeface
