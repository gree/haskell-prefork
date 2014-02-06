
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

relaunchSettings :: (Ord w, Eq w) => PreforkResource w -> (w -> IO (ProcessID)) -> PreforkSettings sc
relaunchSettings resouce forkAction = defaultSettings {
      -- psUpdateConfig = updateConfig
      psUpdateServer = updateWorkers resouce forkAction
    , psCleanupChild = cleanupChild resouce
    , psOnChildFinished = relaunchWorkers resouce forkAction
  }

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
relaunchWorkers resource forkAction _config = do
  let procs = prProcs resource
  let workers = prWorkers resource
  live <- readTVarIO procs
  workers <- readTVarIO workers
  newPids <- fmap M.fromList $ forM (S.toList workers \\ M.elems live) $ \w -> do
    pid <- forkAction w
    -- pid <- forkWorkerProcessWithArgs w ["id=" ++ show (wId w) ]
    return (pid, w)
  atomically $ modifyTVar' procs $ M.union newPids
  return (M.keys newPids)

sendSignal :: Signal -> ProcessID -> IO ()
sendSignal sig cid = signalProcess sig cid `catch` ignore
  where
    ignore :: SomeException -> IO ()
    ignore _ = return ()

