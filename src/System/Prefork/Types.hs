
module System.Prefork.Types (
    PreforkSettings(..)
  , PreforkResource(..)
  , makePreforkResource
  ) where

import qualified Data.Set as S
import qualified Data.Map as M
import System.Posix (ProcessID)
import System.Prefork.Class
import Control.Concurrent.STM
import Control.Applicative

data PreforkSettings sc = PreforkSettings {
    psOnTerminate     :: sc -> [ProcessID] -> IO ()
  , psOnInterrupt     :: sc -> [ProcessID] -> IO ()
  , psOnQuit          :: sc -> IO ()
  , psOnChildFinished :: sc -> IO ([ProcessID])
  , psOnStart         :: Maybe sc -> IO ()
  , psOnFinish        :: Maybe sc -> IO ()
  , psUpdateServer    :: sc -> IO ([ProcessID])
  , psCleanupChild    :: sc -> ProcessID -> IO ()
  , psUpdateConfig    :: IO (Maybe sc)
  }

data PreforkResource w = PreforkResource {
    prProcs   :: TVar (M.Map ProcessID w)
  , prWorkers :: TVar (S.Set w)
  }

makePreforkResource :: (Ord w, Eq w) => [w] -> IO (PreforkResource w)
makePreforkResource workers = PreforkResource <$> newTVarIO M.empty
                                              <*> newTVarIO (S.fromList workers)
