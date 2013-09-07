
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
import System.Environment (getArgs)

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
  , pReadConfigFn :: IO (so, String)
  , pCtrlChan     :: !ControlTChan
  , pProcs        :: !ProcessMapTVar
  }


defaultMain :: (Show so, Read so) => IO (so, String) -> (so -> IO ()) -> IO ()
defaultMain readConfigFn workerAction = do
  args <- getArgs
  case args of 
    x:_ | x == "server" -> do
      workerMain workerAction
    _ -> do
      ctrlChan <- newTChanIO
      procs <- newTVarIO M.empty
      (sopt, _) <- readConfigFn
      soptVar <- newTVarIO sopt
      masterMainLoop (Prefork soptVar readConfigFn ctrlChan procs) False

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
      finRequested <- dispatch msg cids opt
      childIds <- fmap M.keys $ readTVarIO (pProcs prefork)
      unless ((finishing || finRequested) && null childIds) $ loop (finishing || finRequested)

    dispatch msg cids opt = case msg of
      TerminateCM -> do
        mapM_ (sendSignal sigTERM) cids
        return (True)
      InterruptCM -> do
        mapM_ (sendSignal sigINT) cids
        return (True)
      HungupCM -> do
        -- updateServer fs configFile
        return (False)
      QuitCM -> do
        return (False)
      ChildCM -> do
        cleanupChildren opt cids (pProcs prefork)
        return (False)

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
