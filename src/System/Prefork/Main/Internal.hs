
module System.Prefork.Main.Internal (
    compatMain
  , masterMain
  , workerMain
  ) where

import Data.Maybe (listToMaybe, catMaybes)
import Control.Monad (unless, forM_, void)
import Control.Concurrent.STM
import System.Posix
import System.Environment (getArgs)
import System.Exit (exitSuccess)

import System.Prefork.Class
import System.Prefork.Types
import System.Prefork.Worker

data ControlMessage = 
    TerminateCM
  | InterruptCM
  | HungupCM
  | QuitCM
  | ChildCM
  deriving (Eq, Show, Read)

data Prefork sc = Prefork {
    pServerConfig :: !(TVar (Maybe sc))
  , pCtrlChan     :: !(TChan ControlMessage)
  , pProcs        :: !(TVar [ProcessID])
  , pSettings     :: !(PreforkSettings sc)
  }

compatMain :: (WorkerContext w) => PreforkSettings sc -> (w -> IO ()) -> IO ()
compatMain settings workerAction = do
  args <- getArgs
  case (listToMaybe args) of 
    Just x | x == "server" -> workerMain workerAction
    _ -> masterMain settings

workerMain :: (WorkerContext so) => (so -> IO ()) -> IO ()
workerMain act = do
  rawOpt <- getLine
  act $ decodeFromString rawOpt
  exitSuccess

masterMain :: PreforkSettings sc -> IO ()
masterMain settings = do
  ctrlChan  <- newTChanIO
  procs     <- newTVarIO []
  mConfig   <- psUpdateConfig settings
  soptVar   <- newTVarIO mConfig
  atomically $ writeTChan ctrlChan HungupCM  
  setupServer ctrlChan
  (psOnStart settings) mConfig
  masterMainLoop (Prefork soptVar ctrlChan procs settings)
  mConfig' <- readTVarIO soptVar
  (psOnFinish settings) mConfig'
  return ()

masterMainLoop :: Prefork sc -> IO ()
masterMainLoop prefork@Prefork { pSettings = settings } = loop False
  where
    loop :: Bool -> IO ()
    loop finishing = do
      (msg, cids) <- atomically $ do
        msg <- readTChan $ pCtrlChan prefork
        procs <- readTVar $ pProcs prefork
        return (msg, procs)
      finRequested <- dispatch msg cids finishing
      childIds <- readTVarIO (pProcs prefork)
      unless ((finishing || finRequested) && null childIds) $ loop (finishing || finRequested)

    dispatch :: ControlMessage -> [CPid] -> Bool -> IO (Bool)
    dispatch msg cids finishing = case msg of
      TerminateCM -> do
        m <- readTVarIO $ pServerConfig prefork
        whenJust m $ flip (psOnTerminate settings) cids
        return (True)
      InterruptCM -> do
        m <- readTVarIO $ pServerConfig prefork
        whenJust m $ flip (psOnInterrupt settings) cids
        return (True)
      HungupCM -> do
        mConfig <- psUpdateConfig (pSettings prefork)
        whenJust mConfig $ \config -> do
          newProcs <- (psUpdateServer (pSettings prefork)) config
          atomically $ do
            writeTVar (pServerConfig prefork) (Just config)
            modifyTVar' (pProcs prefork) $ (++) newProcs
        return (False)
      QuitCM -> do
        m <- readTVarIO $ pServerConfig prefork
        whenJust m $ psOnQuit settings
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

whenJust :: Monad m => Maybe a -> (a -> m ()) -> m ()
whenJust mg f = maybe (return ()) f mg

cleanupChildren :: [ProcessID] -> TVar [ProcessID] -> IO ([ProcessID])
cleanupChildren cids procs = do
  r <- mapM (getProcessStatus False False) cids -- WNOHANG is true, WUNTRACED is false
  let finished = catMaybes $ flip map (zip cids r) $ checkFinished
  atomically $ modifyTVar' procs $ filter (flip notElem finished)
  return (finished)
  where
    checkFinished (pid, x) = case x of
      Just (Exited _exitCode) -> Just pid
      Just (Terminated _signal) -> Just pid
      Just (Stopped _signal) -> Nothing
      _ -> Nothing

setupServer :: TChan ControlMessage -> IO ()
setupServer chan = do
  let delegate = \sig msg -> setSignalHandler sig $ Catch $ atomically $ writeTChan chan msg
  delegate sigCHLD ChildCM
  delegate sigTERM TerminateCM
  delegate sigINT  InterruptCM
  delegate sigQUIT QuitCM
  delegate sigHUP  HungupCM
  setSignalHandler sigPIPE $ Ignore
  return ()
  where
    setSignalHandler :: Signal -> System.Posix.Handler -> IO ()
    setSignalHandler sig func = void $ installHandler sig func Nothing

