
{-# LANGUAGE NoMonomorphismRestriction #-}

{- | This is a library for servers based on worker process model.
-}
module System.Prefork(
    PreforkSettings(..)
  , defaultMain
  , compatMain
  , defaultTerminateHandler
  , defaultInterruptHandler
  , defaultHungupHandler
  , defaultChildHandler
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
  , psChildHandler     :: [ProcessID] -> so -> IO ()
  , psUpdateConfigFn   :: IO (so, String)
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
  (sopt, _) <- (psUpdateConfigFn settings)
  soptVar   <- newTVarIO sopt
  setupServer ctrlChan
  masterMainLoop (Prefork soptVar ctrlChan procs settings) False

defaultTerminateHandler cids opt = mapM_ (sendSignal sigTERM) cids

defaultInterruptHandler cids opt = mapM_ (sendSignal sigINT) cids

defaultHungupHandler cids opt = return ()

defaultChildHandler cids opt = return ()

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
        (psTerminateHandler settings) cids opt
        return (True)
      InterruptCM -> do
        (psInterruptHandler settings) cids opt
        return (True)
      HungupCM -> do
        (opt', _) <- (psUpdateConfigFn settings)
        (psHungupHandler settings) cids opt'
        return (False)
      QuitCM -> do
        return (False)
      ChildCM -> do
        pids <- cleanupChildren opt cids (pProcs prefork)
        (psChildHandler settings) pids opt
        return (False)

cleanupChildren :: (Show so, Read so) => so -> [ProcessID] -> ProcessMapTVar -> IO ([ProcessID])
cleanupChildren opt cids procs = do
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

