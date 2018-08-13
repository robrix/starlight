module UI.Font where

import qualified Opentype.Fileformat as O

data Typeface = Typeface { typefaceName :: String, typefaceUnderlying :: O.OpentypeFont }

data Font = Font { fontFace :: Typeface, fontSize :: Float }
