{-# LANGUAGE ScopedTypeVariables #-}
module UI.Font where

import Control.Applicative (liftA2)
import Control.Monad (guard)
import qualified Control.Exception as E
import Data.Bits ((.&.), shiftR)
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
  where (makePath, _) = (foldl (\ (makePath, prev) point -> (makePath . pathFor prev point, point)) (M (V2 x y), p) rest)
        pathFor (O.CurvePoint _ _ True)  (O.CurvePoint _ _ False)   = id
        pathFor (O.CurvePoint _ _ True)  (O.CurvePoint x y True)    = L (V2 x y)
        pathFor (O.CurvePoint x y False) (O.CurvePoint x' y' False) = Q (V2 x y) (V2 (x + ((x' - x) `div` 2)) (y + ((y' - y) `div` 2)))
        pathFor (O.CurvePoint x y False) (O.CurvePoint x' y' True)  = Q (V2 x y) (V2 x' y')

glyphPaths :: Typeface -> O.Glyph Int -> [Path V2 O.FWord]
glyphPaths typeface glyph = fmap contourToPath (O.getScaledContours (typefaceUnderlying typeface) glyph)


pathTriangles :: (Int, V2 O.FWord, V2 O.FWord) -> Path V2 O.FWord -> [(Triangle V2 O.FWord, Bool)]
pathTriangles (count, first, current) p = case p of
  M v rest ->                                                                             pathTriangles (0,          v,     v ) rest
  L v rest
    | count >= 2 -> (Triangle first current v,  True)                                   : pathTriangles (succ count, first, v ) rest
    | otherwise  ->                                                                       pathTriangles (succ count, first, v ) rest
  Q v1 v2 rest
    | count >= 2 -> (Triangle first current v2, True) : (Triangle current v1 v2, False) : pathTriangles (succ count, first, v2) rest
    | otherwise  ->                                     (Triangle current v1 v2, False) : pathTriangles (succ count, first, v2) rest
  Z ->                                                                                    []

triangleVertices :: Triangle V2 O.FWord -> Bool -> [V4 Float]
triangleVertices (Triangle (V2 ax ay) (V2 bx by) (V2 cx cy)) True  = [ V4 (fromIntegral ax) (fromIntegral ay) 0 1, V4 (fromIntegral bx) (fromIntegral by) 0 1, V4 (fromIntegral cx) (fromIntegral cy) 0 1 ]
triangleVertices (Triangle (V2 ax ay) (V2 bx by) (V2 cx cy)) False = [ V4 (fromIntegral ax) (fromIntegral ay) 0 0, V4 (fromIntegral bx) (fromIntegral by) 0.5 0, V4 (fromIntegral cx) (fromIntegral cy) 1 1 ]

glyphVertices :: Typeface -> O.Glyph Int -> [V4 Float]
glyphVertices typeface = (>>= uncurry triangleVertices) . (>>= pathTriangles (0, V2 0 0, V2 0 0)) . glyphPaths typeface


encodePath :: Path V2 O.FWord -> [Word8]
encodePath = go . fmap (word16Bytes . fromIntegral)
  where go path = case path of
          M (V2 x y)            rest -> moveTo  : x ++ y             ++ go rest
          L (V2 x y)            rest -> lineTo  : x ++ y             ++ go rest
          Q (V2 x y) (V2 x' y') rest -> curveTo : x ++ y ++ x' ++ y' ++ go rest
          _                          -> close   : []
        (moveTo, lineTo, curveTo, close) = (0, 1, 2, 3)

word16Bytes :: Word16 -> [Word8]
word16Bytes x = [ fromIntegral $ x .&. 0xFF, fromIntegral $ (x .&. 0xFF00) `shiftR` 8 ]

word32Bytes :: Word32 -> [Word8]
word32Bytes x = [ fromIntegral $ x .&. 0xFF, fromIntegral $ (x .&. 0xFF00) `shiftR` 8, fromIntegral $ (x .&. 0xFF0000) `shiftR` 16, fromIntegral $ (x .&. 0xFF000000) `shiftR` 24 ]


encodeGlyphPaths :: Typeface -> O.Glyph Int -> [Word8]
encodeGlyphPaths typeface = (>>= encodePath) . glyphPaths typeface


encodeGlyphsForChars :: Typeface -> [Char] -> [Word8]
encodeGlyphsForChars face chars = header ++ glyphHeaders ++ (charsGlyphsAndPaths >>= \ (_, _, path) -> path)
  where charsGlyphsAndPaths = zip chars (glyphsForChars face chars) >>= \ (char, glyph) -> (,,) char <$> toList glyph <*> fmap (encodeGlyphPaths face) (toList glyph)
        header = word16Bytes (unitsPerEm face) ++ word16Bytes (fromIntegral (ascent face)) ++ word16Bytes (fromIntegral (descent face)) ++ word16Bytes (fromIntegral (length charsGlyphsAndPaths))
        glyphHeaders = snd (foldl encodeGlyphHeader (0, id) charsGlyphsAndPaths) []
        encodeGlyphHeader (offset, makeList) (char, glyph, path) = (offset + fromIntegral (length path), makeList . (++ (word16Bytes (fromIntegral (ord char)) ++ word16Bytes (O.advanceWidth glyph) ++ word32Bytes offset ++ word16Bytes (fromIntegral (length path)))))
