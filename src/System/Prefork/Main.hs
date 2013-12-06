
{-# LANGUAGE NoMonomorphismRestriction #-}

{- | This is a library for servers based on worker process model.
-}
module System.Prefork.Main (
    defaultMain
  , compatMain
  , defaultSettings
  ) where

import Prelude hiding (catch)
import Data.List
import Data.Map (Map)
import Data.Maybe
import qualified Data.Map as M
import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import System.Posix hiding (version)
import System.Exit
import System.Environment (getArgs, lookupEnv)
import System.Posix.Env (setEnv)

import System.Prefork.Class
import System.Prefork.Types
import System.Prefork.Worker
import System.Prefork.Util

data ControlMessage = 
    TerminateCM
  | InterruptCM
  | HungupCM
  | QuitCM
  | ChildCM
  deriving (Eq, Show, Read)

type ControlTChan   = TChan ControlMessage

data Prefork sc = Prefork {
    pServerConfig :: !(TVar (Maybe sc))
  , pCtrlChan     :: !ControlTChan
  , pProcs        :: !(TVar [ProcessID])
  , pSettings     :: !(PreforkSettings sc)
  }

defaultMain :: (WorkerContext so) => PreforkSettings sc -> (so -> IO ()) -> IO ()
defaultMain settings workerAction = do
  mPrefork <- lookupEnv preforkEnvKey
  case mPrefork of
    Just _ -> workerMain workerAction
    Nothing -> do
      setEnv preforkEnvKey "server" True
      masterMain settings

compatMain :: (WorkerContext so) => PreforkSettings sc -> (so -> IO ()) -> IO ()
compatMain settings workerAction = do
  args <- getArgs
  case (listToMaybe args) of 
    Just x | x == "server" -> workerMain workerAction
    _ -> masterMain settings

masterMain :: PreforkSettings sc -> IO ()
masterMain settings = do
  ctrlChan  <- newTChanIO
  procs     <- newTVarIO []
  mso       <- psUpdateConfig settings
  soptVar   <- newTVarIO mso
  let prefork = Prefork soptVar ctrlChan procs settings
  setupServer ctrlChan
  atomically $ writeTChan ctrlChan HungupCM  
  masterMainLoop prefork

defaultSettings :: PreforkSettings sc
defaultSettings = PreforkSettings {
    psOnTerminate      = \_ -> mapM_ (sendSignal sigTERM)
  , psOnInterrupt      = \_ -> mapM_ (sendSignal sigINT)
  , psOnQuit           = \_ -> return ()
  , psOnChildFinished  = \_ -> return ([])
  , psUpdateServer     = \_ -> return ([])
  , psCleanupChild     = \_ pid -> return ()
  , psUpdateConfig     = return (Nothing)
  }

masterMainLoop :: Prefork sc -> IO ()
masterMainLoop prefork@Prefork { pSettings = settings } = loop False
  where
    loop finishing = do
      (msg, cids) <- atomically $ do
        msg <- readTChan $ pCtrlChan prefork
        procs <- readTVar $ pProcs prefork
        return (msg, procs)
      finRequested <- dispatch msg cids finishing
      childIds <- readTVarIO (pProcs prefork)
      unless ((finishing || finRequested) && null childIds) $ loop (finishing || finRequested)

    dispatch msg cids finishing = case msg of
      TerminateCM -> do
        m <- readTVarIO $ pServerConfig prefork
        maybe (return ()) (flip (psOnTerminate settings) cids) m
        return (True)
      InterruptCM -> do
        m <- readTVarIO $ pServerConfig prefork
        maybe (return ()) (flip (psOnInterrupt settings) cids) m
        return (True)
      HungupCM -> do
        mConfig <- psUpdateConfig (pSettings prefork)
        flip (maybe (return ())) mConfig $ \config -> do
          newProcs <- (psUpdateServer (pSettings prefork)) config
          atomically $ do
            writeTVar (pServerConfig prefork) mConfig
            modifyTVar' (pProcs prefork) $ (++) newProcs
        return (False)
      QuitCM -> do
        m <- readTVarIO $ pServerConfig prefork
        maybe (return ()) (psOnQuit settings) m
        return (False)
      ChildCM -> do
        finished <- cleanupChildren cids (pProcs prefork)
        mConfig <- readTVarIO $ pServerConfig prefork
        case mConfig of
          Just config -> do
            forM_ finished $ (psCleanupChild settings) config
            unless finishing $ do
              newProcs <- (psOnChildFinished settings) config
              atomically $ modifyTVar' (pProcs prefork) $ (++) newProcs
          Nothing -> return ()
        return (False)

cleanupChildren :: [ProcessID] -> TVar [ProcessID] -> IO ([ProcessID])
cleanupChildren cids procs = do
  r <- mapM (getProcessStatus False False) cids -- WNOHANG is true, WUNTRACED is false
  let finished = catMaybes $ flip map (zip cids r) $ \x -> case x of
                                                             (pid, Just (Exited _exitCode)) -> Just pid
                                                             (pid, Just (Terminated _signal)) -> Just pid
                                                             (pid, Just (Stopped _signal)) -> Nothing
                                                             _ -> Nothing
  atomically $ do
    modifyTVar' procs $ filter (\v -> v `notElem` finished)
  return (finished)

setupServer :: ControlTChan -> IO ()
setupServer chan = do
  setSignalHandler sigCHLD $ Catch $ do
    atomically $ writeTChan chan ChildCM
  setSignalHandler sigTERM $ Catch $ do
    atomically $ writeTChan chan TerminateCM
  setSignalHandler sigINT $ Catch $ do
    atomically $ writeTChan chan InterruptCM
  setSignalHandler sigQUIT $ Catch $ do
    atomically $ writeTChan chan QuitCM
  setSignalHandler sigHUP $ Catch $ do
    atomically $ writeTChan chan HungupCM
  setSignalHandler sigPIPE $ Ignore
  return ()

