module GL.Carrier.Check.Identity
( -- * Check carrier
  runCheck
, CheckC(CheckC)
) where

newtype CheckC m a = CheckC { runCheck :: m a }
