
module System.Prefork.Types (
    PreforkSettings(..)
  ) where

import System.Posix hiding (version)

data PreforkSettings sc = PreforkSettings {
    psOnTerminate     :: sc -> [ProcessID] -> IO ()
  , psOnInterrupt     :: sc -> [ProcessID] -> IO ()
  , psOnChildFinished :: sc -> IO ([ProcessID])
  , psUpdateServer    :: sc -> IO ([ProcessID])
  , psCleanupChild    :: sc -> ProcessID -> IO ()
  , psUpdateConfig    :: IO (Maybe sc)
  }
