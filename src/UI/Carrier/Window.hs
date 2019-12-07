module UI.Carrier.Window
( -- * Window carrier
  WindowC(..)
  -- * Window effect
, module UI.Effect.Window
) where

import UI.Effect.Window

newtype WindowC m a = WindowC (m a)
