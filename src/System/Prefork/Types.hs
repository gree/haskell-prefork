
module System.Prefork.Types (
    PreforkSettings(..)
  , PreforkResource(..)
  , makePreforkResource
  , emptyPreforkResource
  , updateWorkerSet
  ) where

import qualified Data.Set as S
import qualified Data.Map as M
import System.Posix (ProcessID)
import System.Prefork.Class
import Control.Concurrent.STM
import Control.Applicative

{- | This represents handlers for controlling child processes.
     
     The type parameter 'sc' is a server configuration (application specific).
-}
data PreforkSettings sc = PreforkSettings {
    psOnTerminate     :: sc -> [ProcessID] -> IO () -- ^ This is called on TERM  signal.
  , psOnInterrupt     :: sc -> [ProcessID] -> IO () -- ^ This is called on INT   signal.
  , psOnQuit          :: sc -> IO ()                -- ^ This is called on QUIT  signal.
  , psOnChildFinished :: sc -> IO ([ProcessID])     -- ^ This is called on CHILD signal.
  , psOnStart         :: Maybe sc -> IO ()          -- ^ This is called on the start of a parent.
  , psOnFinish        :: Maybe sc -> IO ()          -- ^ This is called on the finish of a parent.
  , psUpdateServer    :: sc -> IO ([ProcessID])     -- ^ This is called when the server state has changed.
  , psCleanupChild    :: sc -> ProcessID -> IO ()   -- ^ This is called when one of child process has finished.
  , psUpdateConfig    :: IO (Maybe sc)              -- ^ This is called when a configuration is needed.
  }

data PreforkResource w = PreforkResource {
    prProcs   :: TVar (M.Map ProcessID w)
  , prWorkers :: TVar (S.Set w)
  }

makePreforkResource :: (Ord w, Eq w) => [w] -> IO (PreforkResource w)
makePreforkResource workers = PreforkResource <$> newTVarIO M.empty
                                              <*> newTVarIO (S.fromList workers)

emptyPreforkResource :: (Ord w, Eq w) => IO (PreforkResource w)
emptyPreforkResource = makePreforkResource []

updateWorkerSet :: (Ord w, Eq w) => PreforkResource w -> [w] -> IO ()
updateWorkerSet resource workers = atomically $ writeTVar (prWorkers resource) $ S.fromList workers
