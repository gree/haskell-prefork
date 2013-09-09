
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

-- import System.Prefork.Class
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

data Prefork sc = Prefork {
  --  pServerOption :: !(TVar (Maybe so))
    pServerConfig :: !(TVar (Maybe sc))
  , pCtrlChan     :: !ControlTChan
  , pProcs        :: !ProcessMapTVar
  , pSettings     :: PreforkSettings sc
  }

data PreforkSettings sc = PreforkSettings {
    psTerminateHandler    :: sc -> [ProcessID] -> IO ()
  , psInterruptHandler    :: sc -> [ProcessID] -> IO ()
  , psHungupHandler       :: sc -> [ProcessID] -> IO ()
  , psCleanupChild        :: sc -> ProcessID -> IO ()
  , psUpdateConfig        :: IO (Maybe sc)
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
  procs     <- newTVarIO M.empty
  mso       <- psUpdateConfig settings
  soptVar   <- newTVarIO mso
  setupServer ctrlChan
  masterMainLoop (Prefork soptVar ctrlChan procs settings)

defaultSettings = PreforkSettings {
    psTerminateHandler = \config -> mapM_ (sendSignal sigTERM)
  , psInterruptHandler = \config -> mapM_ (sendSignal sigINT)
  , psHungupHandler    = \config pids -> return ()
  , psCleanupChild     = \config pid -> return ()
  , psUpdateConfig     = return (Nothing)
  }

masterMainLoop :: Prefork sc -> IO ()
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
        m <- readTVarIO $ pServerConfig prefork
        maybe (return ()) (flip (psTerminateHandler settings) cids) m
        return (True)
      InterruptCM -> do
        m <- readTVarIO $ pServerConfig prefork
        maybe (return ()) (flip (psInterruptHandler settings) cids) m
        return (True)
      HungupCM -> do
        mConfig <- psUpdateConfig settings
        flip (maybe (return ())) mConfig $ \config -> do
          atomically $ writeTVar (pServerConfig prefork) mConfig
          (psHungupHandler settings) config cids 
        return (False)
      QuitCM -> do
        return (False)
      ChildCM -> do
        finished <- cleanupChildren cids (pProcs prefork)
        mConfig <- readTVarIO $ pServerConfig prefork
        case mConfig of
          Just config -> do
            forM_ finished $ \pid -> do
              (psCleanupChild settings) config pid
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

