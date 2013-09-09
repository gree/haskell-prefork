
{-# LANGUAGE NoMonomorphismRestriction #-}

{- | This is a library for servers based on worker process model.
-}
module System.Prefork(
    PreforkSettings(..)
  , defaultMain
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
-- import Data.Default (Default, def)

import System.Prefork.Worker

data ControlMessage = 
    TerminateCM
  | InterruptCM
  | HungupCM
  | QuitCM
  | ChildCM
  deriving (Eq, Show, Read)

type ControlTChan   = TChan ControlMessage
type ProcessMapTVar = TVar (Map ProcessID String)

data (Show so, Read so) => Prefork so = Prefork {
    pServerOption :: !(TVar (Maybe so))
  , pCtrlChan     :: !ControlTChan
  , pProcs        :: !ProcessMapTVar
  , pSettings     :: PreforkSettings so
  }

data (Show so, Read so) => PreforkSettings so = PreforkSettings {
    psTerminateHandler    :: [ProcessID] -> so -> IO ()
  , psInterruptHandler    :: [ProcessID] -> so -> IO ()
  , psHungupHandler       :: [ProcessID] -> so -> IO ()
  , psCleanupChild        :: ProcessID -> so -> IO ()
  , psUpdateConfig        :: IO (Maybe so)
  }

defaultMain :: (Show so, Read so) => PreforkSettings so -> (so -> IO ()) -> IO ()
defaultMain settings workerAction = do
  mPrefork <- lookupEnv envPrefork
  case mPrefork of
    Just _ -> workerMain workerAction
    Nothing -> masterMain settings

compatMain :: (Show so, Read so) => PreforkSettings so -> (so -> IO ()) -> IO ()
compatMain settings workerAction = do
  args <- getArgs
  case args of 
    x:_ | x == "server" -> workerMain workerAction
    _ -> masterMain settings

masterMain :: (Show so, Read so) => PreforkSettings so -> IO ()
masterMain settings = do
  ctrlChan  <- newTChanIO
  procs     <- newTVarIO M.empty
  mso       <- psUpdateConfig settings
  soptVar   <- newTVarIO mso
  setupServer ctrlChan
  masterMainLoop (Prefork soptVar ctrlChan procs settings)

defaultSettings = PreforkSettings {
    psTerminateHandler = \pids opt -> mapM_ (sendSignal sigTERM) pids
  , psInterruptHandler = \pids opt -> mapM_ (sendSignal sigINT) pids
  , psHungupHandler    = \pids opt -> return ()
  , psCleanupChild     = \pid opt -> return ()
  , psUpdateConfig     = return (Nothing)
  }

masterMainLoop :: (Show so, Read so) => Prefork so -> IO ()
masterMainLoop prefork@Prefork { pSettings = settings } = do
  setHandler sigCHLD $ childHandler (pCtrlChan prefork)
  loop False
  where
    loop finishing = do
      (msg, cids) <- atomically $ do
        msg <- readTChan $ pCtrlChan prefork
        procs <- readTVar $ pProcs prefork
        return (msg, M.keys procs)
      finRequested <- dispatch msg cids
      childIds <- fmap M.keys $ readTVarIO (pProcs prefork)
      unless ((finishing || finRequested) && null childIds) $ loop (finishing || finRequested)

    dispatch msg cids = case msg of
      TerminateCM -> do
        mopt <- readTVarIO $ pServerOption prefork
        maybe (return ()) (\opt -> (psTerminateHandler settings) cids opt) mopt
        return (True)
      InterruptCM -> do
        mopt <- readTVarIO $ pServerOption prefork
        maybe (return ()) (\opt -> (psInterruptHandler settings) cids opt) mopt
        return (True)
      HungupCM -> do
        mopt' <- psUpdateConfig settings
        case mopt' of
          Just opt' -> do
            atomically $ writeTVar (pServerOption prefork) mopt'
            (psHungupHandler settings) cids opt'
          Nothing -> return ()
        return (False)
      QuitCM -> do
        return (False)
      ChildCM -> do
        finished <- cleanupChildren cids (pProcs prefork)
        mopt <- readTVarIO $ pServerOption prefork
        case mopt of
          Just opt -> do
            forM_ finished $ \pid -> do
              (psCleanupChild settings) pid opt
          Nothing -> return ()
        return (False)

cleanupChildren :: [ProcessID] -> ProcessMapTVar -> IO ([ProcessID])
cleanupChildren cids procs = do
  r <- mapM (getProcessStatus False False) cids
  let finished = catMaybes $ flip map (zip cids r) $ \x -> case x of
                                                             (pid, Just _exitCode) -> Just pid
                                                             _ -> Nothing
  atomically $ do
    modifyTVar' procs $ M.filterWithKey (\k _v -> k `notElem` finished)
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

