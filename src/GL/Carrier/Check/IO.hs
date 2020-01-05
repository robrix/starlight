module GL.Carrier.Check.IO
( -- * Check carrier
  runCheck
, CheckC(CheckC)
) where

newtype CheckC m a = CheckC { runCheck :: m a }
