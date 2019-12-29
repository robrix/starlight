{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module UI.Typeface
( Typeface(name)
, Font(..)
, fontScale
, readTypeface
, readFontOfSize
, cacheCharactersForDrawing
, layoutString
, drawingGlyphs
) where

import           Control.Effect.Finally
import           Control.Monad (guard, join, (<=<))
import           Control.Monad.IO.Class.Lift
import           Data.Bifunctor (first)
import           Data.Char (isPrint, isSeparator, ord)
import           Data.Coerce (coerce)
import           Data.Foldable (find, foldl')
import           Data.Functor.I (I(..))
import           Data.Functor.Interval (Interval(..))
import           Data.IORef
import qualified Data.Map as Map
import           Data.Maybe (catMaybes)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.Vector ((!?))
import           Geometry.Triangle
import           GHC.Stack
import           GL.Array
import           GL.Buffer as B
import           GL.Framebuffer
import           GL.Object
import           GL.Program
import           Lens.Micro
import           Linear.V2
import qualified Opentype.Fileformat as O
import           UI.Glyph
import qualified UI.Label.Glyph as Glyph
import           UI.Path

data Typeface = Typeface
  { name         :: Maybe String
  , allGlyphs    :: Map.Map Char (Maybe Glyph)
  , opentypeFont :: O.OpentypeFont
  , glyphP       :: Program Glyph.U Glyph.V Glyph.O
  , glyphB       :: Buffer 'B.Array (Glyph.V I)
  , glyphA       :: Array (Glyph.V I)
  , rangesRef    :: IORef (Map.Map Char (Interval I Int))
  }

data Font = Font
  { face :: Typeface
  , size :: Float
  }

fontScale :: Font -> Float
fontScale (Font face size) = size * scale where
  scale = 1 / fromIntegral (O.unitsPerEm (O.headTable (opentypeFont face)))


readTypeface
  :: ( HasCallStack
     , Has Finally sig m
     , Has (Lift IO) sig m
     )
  => FilePath
  -> m Typeface
readTypeface path = do
  o <- sendM (O.readOTFile path)

  glyphP <- build Glyph.shader
  glyphA <- gen1
  glyphB <- gen1

  let allGlyphs = Map.fromList (map ((,) <*> lookupGlyph) [minBound..maxBound])
      lookupGlyph char = do
        table <- O.glyphMap <$> cmap
        glyphID <- table Map.!? fromIntegral (ord char)
        glyphTable <- glyphTable
        g <- glyphTable !? fromIntegral glyphID
        let vertices = if isPrint char && not (isSeparator char) then glyphVertices g else []
        pure $! Glyph char (fromIntegral (O.advanceWidth g)) vertices (bounds (map (^. _xy) vertices))
      cmap = find supportedPlatform (O.getCmaps (O.cmapTable o))
      glyphTable = case O.outlineTables o of
        O.QuadTables _ (O.GlyfTable glyphs) -> Just glyphs
        _                                   -> Nothing
      supportedPlatform p = O.cmapPlatform p == O.UnicodePlatform || O.cmapPlatform p == O.MicrosoftPlatform && O.cmapEncoding p == 1
      glyphVertices = uncurry triangleVertices . first (fmap fromIntegral) <=< pathTriangles <=< map contourToPath . O.getScaledContours o

  rangesRef <- sendM (newIORef Map.empty)

  pure Typeface
    { name = opentypeFontName o
    , allGlyphs
    , opentypeFont = o
    , glyphP
    , glyphA
    , glyphB
    , rangesRef
    }

readFontOfSize
  :: ( HasCallStack
     , Has Finally sig m
     , Has (Lift IO) sig m
     )
  => FilePath
  -> Float
  -> m Font
readFontOfSize path size = (`Font` size) <$> readTypeface path


cacheCharactersForDrawing :: Has (Lift IO) sig m => Typeface -> String -> m ()
cacheCharactersForDrawing Typeface{ allGlyphs, glyphA, glyphB, rangesRef } string = do
  let (vs, ranges, _) = foldl' combine (id, Map.empty, 0) (glyphsForString allGlyphs string)
      combine (vs, cs, i) Glyph{ char, geometry } = let i' = i + I (length geometry) in (vs . (geometry ++), Map.insert char (Interval i i') cs, i')
      vertices = vs []

  bind (Just glyphB)
  realloc glyphB (length vertices) Static Draw
  copy glyphB 0 (coerce vertices)

  bindArray glyphA $ configureArray glyphB glyphA

  sendM (writeIORef rangesRef ranges)


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
safeToEnum n = toEnum n <$ guard (n < fromEnum (maxBound @n) && n > fromEnum (minBound @n))


layoutString :: Has (Lift IO) sig m => Typeface -> String -> m Run
layoutString face string = do
  ranges <- sendM (readIORef (rangesRef face))
  pure (layoutGlyphs ranges (glyphsForString (allGlyphs face) string))

glyphsForString :: Map.Map Char (Maybe Glyph) -> String -> [Glyph]
glyphsForString allGlyphs = catMaybes . map (join . (allGlyphs Map.!?))


drawingGlyphs :: Has (Lift IO) sig m => Typeface -> ProgramT Glyph.U Glyph.V Glyph.O (ArrayT Glyph.V m) a -> m a
drawingGlyphs Typeface{ glyphP, glyphA } = bindArray glyphA . use glyphP


contourToPath :: [O.CurvePoint] -> Path V2 O.FWord
contourToPath [] = []
contourToPath (p@(O.CurvePoint x y _) : ps) = M (V2 x y) : go p ps
  where go (O.CurvePoint _  _  True)  (p@(O.CurvePoint _  _  False) : ps) = go p ps
        go (O.CurvePoint _  _  True)  (p@(O.CurvePoint x  y  True)  : ps) = L (V2 x y) : go p ps
        go (O.CurvePoint x1 y1 False) (p@(O.CurvePoint x2 y2 False) : ps) = Q (V2 x1 y1) (V2 (x1 + ((x2 - x1) `div` 2)) (y1 + ((y2 - y1) `div` 2))) : go p ps
        go (O.CurvePoint x1 y1 False) (p@(O.CurvePoint x2 y2 True)  : ps) = Q (V2 x1 y1) (V2 x2 y2) : go p ps
        go (O.CurvePoint x1 y1 False) []                                  = Q (V2 x1 y1) (V2 x y) : []
        go _                          []                                  = []
