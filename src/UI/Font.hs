{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
module UI.Font where

import Control.Applicative (liftA2)
import Control.Monad (guard)
import qualified Control.Exception as E
import Data.Bifunctor (first)
import Data.Bytes
import Data.Char (ord)
import Data.Foldable (find, toList)
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
import Linear.Vector (zero)
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
contourToPath (p@(O.CurvePoint x y _) : rest) = makePath Z
  where (makePath, _) = foldl (\ (makePath, prev) point -> (makePath . pathFor prev point, point)) (M (V2 x y), p) rest
        pathFor (O.CurvePoint _ _ True)  (O.CurvePoint _ _ False)   = id
        pathFor (O.CurvePoint _ _ True)  (O.CurvePoint x y True)    = L (V2 x y)
        pathFor (O.CurvePoint x y False) (O.CurvePoint x' y' False) = Q (V2 x y) (V2 (x + ((x' - x) `div` 2)) (y + ((y' - y) `div` 2)))
        pathFor (O.CurvePoint x y False) (O.CurvePoint x' y' True)  = Q (V2 x y) (V2 x' y')

glyphPaths :: Typeface -> O.Glyph Int -> [Path V2 O.FWord]
glyphPaths typeface glyph = fmap contourToPath (O.getScaledContours (typefaceUnderlying typeface) glyph)


glyphVertices :: Typeface -> O.Glyph Int -> [V4 Float]
glyphVertices typeface = (>>= uncurry triangleVertices . first (fmap fromIntegral)) . (>>= pathTriangles 0 zero zero) . glyphPaths typeface


encodeGlyphPaths :: Typeface -> O.Glyph Int -> [Word8]
encodeGlyphPaths typeface = (>>= encodePath) . glyphPaths typeface


encodeGlyphsForChars :: Typeface -> [Char] -> [Word8]
encodeGlyphsForChars face chars = header ++ glyphHeaders ++ (charsGlyphsAndPaths >>= \ (_, _, path) -> path)
  where charsGlyphsAndPaths = zip chars (glyphsForChars face chars) >>= \ (char, glyph) -> (,,) char <$> toList glyph <*> fmap (encodeGlyphPaths face) (toList glyph)
        header = toBytes (unitsPerEm face) ++ toBytes @Word16 (fromIntegral (ascent face)) ++ toBytes @Word16 (fromIntegral (descent face)) ++ toBytes @Word16 (fromIntegral (length charsGlyphsAndPaths))
        glyphHeaders = snd (foldl encodeGlyphHeader (0, id) charsGlyphsAndPaths) []
        encodeGlyphHeader (offset, makeList) (char, glyph, path) = (offset + fromIntegral (length path), makeList . (++ (toBytes @Word16 (fromIntegral (ord char)) ++ toBytes (O.advanceWidth glyph) ++ toBytes @Word32 offset ++ toBytes @Word16 (fromIntegral (length path)))))
