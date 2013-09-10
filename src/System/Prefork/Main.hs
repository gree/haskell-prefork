
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
import System.Environment (getArgs, lookupEnv)

-- import System.Prefork.Class
import System.Prefork.Types
import System.Prefork.Worker

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

defaultMain :: (Show so, Read so) => PreforkSettings sc -> (so -> IO ()) -> IO ()
defaultMain settings workerAction = do
  mPrefork <- lookupEnv envPrefork
  case mPrefork of
    Just _ -> workerMain workerAction
    Nothing -> masterMain settings

compatMain :: (Show so, Read so) => PreforkSettings sc -> (so -> IO ()) -> IO ()
compatMain settings workerAction = do
  mPrefork <- lookupEnv envPrefork
  args <- getArgs
  case (listToMaybe args) of 
    Just x | x == "server" -> workerMain workerAction
    Nothing -> masterMain settings

masterMain :: PreforkSettings sc -> IO ()
masterMain settings = do
  ctrlChan  <- newTChanIO
  procs     <- newTVarIO []
  mso       <- psUpdateConfig settings
  soptVar   <- newTVarIO mso
  let prefork = Prefork soptVar ctrlChan procs settings
  setHandler sigCHLD $ childHandler ctrlChan
  setupServer ctrlChan
  atomically $ writeTChan ctrlChan HungupCM  
  masterMainLoop prefork

defaultSettings :: PreforkSettings sc
defaultSettings = PreforkSettings {
    psTerminateHandler = \config -> mapM_ (sendSignal sigTERM)
  , psInterruptHandler = \config -> mapM_ (sendSignal sigINT)
  , psOnChildFinished  = \config -> return ([])
  , psUpdateServer     = \config -> return ([])
  , psCleanupChild     = \config pid -> return ()
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
        maybe (return ()) (flip (psTerminateHandler settings) cids) m
        return (True)
      InterruptCM -> do
        m <- readTVarIO $ pServerConfig prefork
        maybe (return ()) (flip (psInterruptHandler settings) cids) m
        return (True)
      HungupCM -> do
        mConfig <- psUpdateConfig (pSettings prefork)
        flip (maybe (return ())) mConfig $ \config -> do
          newProcs <- (psUpdateServer (pSettings prefork)) config
          atomically $ do
            writeTVar (pServerConfig prefork) mConfig
            modifyTVar' (pProcs prefork) $ \procs -> procs ++ newProcs
        return (False)
      QuitCM -> do
        return (False)
      ChildCM -> do
        finished <- cleanupChildren cids (pProcs prefork)
        mConfig <- readTVarIO $ pServerConfig prefork
        case mConfig of
          Just config -> do
            forM_ finished $ (psCleanupChild settings) config
            unless finishing $ do
              newProcs <- (psOnChildFinished settings) config
              atomically $ modifyTVar' (pProcs prefork) $ \procs -> procs ++ newProcs
          Nothing -> return ()
        return (False)

cleanupChildren :: [ProcessID] -> TVar [ProcessID] -> IO ([ProcessID])
cleanupChildren cids procs = do
  r <- mapM (getProcessStatus False False) cids
  let finished = catMaybes $ flip map (zip cids r) $ \x -> case x of
                                                             (pid, Just _exitCode) -> Just pid
                                                             _ -> Nothing
  atomically $ do
    modifyTVar' procs $ filter (\v -> v `notElem` finished)
  return (finished)

childHandler :: ControlTChan -> System.Posix.Handler
childHandler ctrlChan = Catch $ do
  atomically $ writeTChan ctrlChan ChildCM

setupServer :: ControlTChan -> IO ()
setupServer chan = do
  setHandler sigTERM $ Catch $ do
    atomically $ writeTChan chan TerminateCM
  setHandler sigINT $ Catch $ do
    atomically $ writeTChan chan InterruptCM
  setHandler sigQUIT $ Catch $ do
    atomically $ writeTChan chan QuitCM
  setHandler sigHUP $ Catch $ do
    atomically $ writeTChan chan HungupCM
  setHandler sigPIPE $ Ignore
  return ()

setHandler :: Signal -> System.Posix.Handler -> IO ()
setHandler sig func = void $ installHandler sig func Nothing

sendSignal :: Signal -> ProcessID -> IO ()
sendSignal sig cid = signalProcess sig cid `catch` ignore
  where
    ignore :: SomeException -> IO ()
    ignore _ = return ()

envPrefork = "PREFORK"

