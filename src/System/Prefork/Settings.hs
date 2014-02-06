
module System.Prefork.Settings (defaultSettings) where

import Control.Exception (SomeException, catch)
import System.Posix

import System.Prefork.Class
import System.Prefork.Types
import System.Prefork.Worker

defaultSettings :: PreforkSettings sc
defaultSettings = PreforkSettings {
    psOnTerminate      = \_config -> mapM_ (sendSignal sigTERM)
  , psOnInterrupt      = \_config -> mapM_ (sendSignal sigINT)
  , psOnQuit           = \_config -> return ()
  , psOnChildFinished  = \_config -> return ([])
  , psOnStart          = \_mConfig -> return ()
  , psOnFinish         = \_mConfig -> return ()
  , psUpdateServer     = \_config -> return ([])
  , psCleanupChild     = \_config _pid -> return ()
  , psUpdateConfig     = return (Nothing)
  }

sendSignal :: Signal -> ProcessID -> IO ()
sendSignal sig cid = signalProcess sig cid `catch` ignore
  where
    ignore :: SomeException -> IO ()
    ignore _ = return ()
