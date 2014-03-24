-- Copyright: (c) 2013 GREE, Inc.
-- License: MIT-style

module System.Prefork.Settings (defaultSettings, relaunchSettings) where

import Control.Exception (SomeException, catch)
import System.Posix

import System.Prefork.Class
import System.Prefork.Types
import System.Prefork.Worker
import Control.Concurrent.STM
import qualified Data.Set as S
import qualified Data.Map as M
import Data.List
import Control.Monad

{- | default settings for defaultMain
     
     This just sends signals to child processes.
-}
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

{- | relaunch settings
     
     This requires 'PreforkResource' that describes resouces used by workers.
     'relaunchSettings' takes two functions.
     The one is 'update' and the other is 'fork'.
     'update' function is used for reading server configuration (usually from a file)
     and update 'sc' type.
     'fork' function is used for launching a new worker with a worker context.
     The worker context should be the instance of 'Eq' and 'Ord' classes because it 
     will be a element type of 'Set'.
-}
relaunchSettings :: (Ord w, Eq w)
                    => PreforkResource w
                    -> (PreforkResource w -> IO (Maybe sc))
                    -> (w -> IO (ProcessID))
                    -> PreforkSettings sc
relaunchSettings resource updateAction forkAction = defaultSettings {
      psUpdateConfig = updateAction resource
    , psUpdateServer = updateWorkers resource forkAction
    , psCleanupChild = cleanupChild resource
    , psOnChildFinished = relaunchWorkers resource forkAction
  }
  where
    -- Clean up application specific resources associated to a child process
    cleanupChild :: (Ord w, Eq w) => PreforkResource w -> sc -> ProcessID -> IO ()
    cleanupChild resource _config pid = atomically $ modifyTVar' (prProcs resource) $ M.delete pid
    
    -- Update the entire state of a server
    updateWorkers :: (Ord w, Eq w) => PreforkResource w -> (w -> IO (ProcessID)) -> sc -> IO ([ProcessID])
    updateWorkers resource forkAction _config = do
      workers <- readTVarIO (prWorkers resource)
      newPids <- forM (S.toList workers) $ \w -> do
        pid <- forkAction w
        return (pid, w)
      oldPids <- atomically $ swapTVar (prProcs resource) (M.fromList newPids)
      forM_ (M.keys oldPids) $ sendSignal sigTERM
      return (map fst newPids)
    
    -- Relaunch workers after some of them terminate
    relaunchWorkers :: (Ord w, Eq w) => PreforkResource w -> (w -> IO (ProcessID)) -> sc -> IO ([ProcessID])
    relaunchWorkers resource@PreforkResource { prProcs = procs, prWorkers = workers } forkAction _config = do
      (live, workers) <- atomically $ do
        live <- readTVar procs
        workers <- readTVar workers
        return (live, workers)
      newPids <- fmap M.fromList $ forM (S.toList workers \\ M.elems live) $ \w -> do
        pid <- forkAction w
        return (pid, w)
      atomically $ modifyTVar' procs $ M.union newPids
      return (M.keys newPids)

-- private
sendSignal :: Signal -> ProcessID -> IO ()
sendSignal sig cid = signalProcess sig cid `catch` ignore
  where
    ignore :: SomeException -> IO ()
    ignore _ = return ()

