
module System.Prefork.Types (
    PreforkSettings(..)
  ) where

import System.Posix hiding (version)

data PreforkSettings sc = PreforkSettings {
    psTerminateHandler    :: sc -> [ProcessID] -> IO ()
  , psInterruptHandler    :: sc -> [ProcessID] -> IO ()
  , psOnChildFinished     :: sc -> IO ([ProcessID])
  , psUpdateServer        :: sc -> IO ([ProcessID])
  , psCleanupChild        :: sc -> ProcessID -> IO ()
  , psUpdateConfig        :: IO (Maybe sc)
  }
