
{-# LANGUAGE NoMonomorphismRestriction #-}

{- | This is a library for servers based on worker process model.
-}
module System.Prefork(defaultMain) where

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
    pServerOption :: !(TVar so)
  , pCtrlChan     :: !ControlTChan
  , pProcs        :: !ProcessMapTVar
  , pSettings     :: PreforkSettings so
  }

data (Show so, Read so) => PreforkSettings so = PreforkSettings {
    psTerminateHandler :: [ProcessID] -> so -> IO ()
  , psInterruptHandler :: [ProcessID] -> so -> IO ()
  , psHungupHandler    :: [ProcessID] -> so -> IO ()
  , psReadConfigFn     :: IO (so, String)
  }

defaultMain :: (Show so, Read so) => IO (so, String) -> (so -> IO ()) -> IO ()
defaultMain readConfigFn workerAction = do
  mPrefork <- lookupEnv envPrefork
  case mPrefork of
    Just _ -> workerMain workerAction
    Nothing -> masterMain readConfigFn

compatMain :: (Show so, Read so) => IO (so, String) -> (so -> IO ()) -> IO ()
compatMain readConfigFn workerAction = do
  args <- getArgs
  case args of 
    x:_ | x == "server" -> workerMain workerAction
    _ -> masterMain readConfigFn

masterMain :: (Show so, Read so) => IO (so, String) -> IO ()
masterMain readConfigFn = do
  ctrlChan <- newTChanIO
  procs <- newTVarIO M.empty
  (sopt, _) <- readConfigFn
  soptVar <- newTVarIO sopt
  let settings = PreforkSettings defaultTerminateHandler defaultInterruptHandler defaultHungupHandler readConfigFn
  masterMainLoop (Prefork soptVar ctrlChan procs settings) False
  
masterMainLoop :: (Show so, Read so) => Prefork so -> Bool -> IO ()
masterMainLoop prefork finishing = do
  setHandler sigCHLD $ childHandler (pCtrlChan prefork)
  loop False
  where
    loop False = do
      (msg, cids, opt) <- atomically $ do
        msg <- readTChan $ pCtrlChan prefork
        procs <- readTVar $ pProcs prefork
        opt <- readTVar $ pServerOption prefork
        return (msg, M.keys procs, opt)
      finRequested <- dispatch (pSettings prefork) msg cids opt
      childIds <- fmap M.keys $ readTVarIO (pProcs prefork)
      unless ((finishing || finRequested) && null childIds) $ loop (finishing || finRequested)

    dispatch settings msg cids opt = case msg of
      TerminateCM -> do
        -- mapM_ (sendSignal sigTERM) cids
        (psTerminateHandler settings) cids opt
        return (True)
      InterruptCM -> do
        -- mapM_ (sendSignal sigINT) cids
        (psInterruptHandler settings) cids opt
        return (True)
      HungupCM -> do
        (opt', _) <- (psReadConfigFn settings)
        (psHungupHandler settings) cids opt'
        return (False)
      QuitCM -> do
        return (False)
      ChildCM -> do
        cleanupChildren opt cids (pProcs prefork)
        return (False)

defaultTerminateHandler cids opt = mapM_ (sendSignal sigTERM) cids

defaultInterruptHandler cids opt = mapM_ (sendSignal sigINT) cids

defaultHungupHandler cids opt = return ()

cleanupChildren opt cids procs = do
  r <- mapM (getProcessStatus False False) cids
  let finished = catMaybes $ flip map (zip cids r) $ \x -> case x of
                                                             (pid, Just _exitCode) -> Just pid
                                                             _ -> Nothing
  atomically $ do
    modifyTVar' procs $ M.filterWithKey (\k _v -> k `notElem` finished)

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

